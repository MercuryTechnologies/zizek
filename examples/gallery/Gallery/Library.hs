-- | A pooled value's lineage, and an elided step that caused the failure.
--
-- A library numbers its shelves from zero and keeps each book on the shelf
-- found by dividing the book's id by the number of shelves and taking the
-- remainder. When the collection outgrows the shelves, the library doubles
-- them and moves every shelved book to its new place. Lending a book writes
-- its shelf on the loan slip, and a returned book goes back on the shelf its
-- slip names. A book that was out on loan when the shelves doubled therefore
-- comes back to its old shelf, where the catalog no longer looks for it.
--
-- The log follows @book₁@ from the shelf onto loan and back. Acquiring
-- @book₂@ is what doubled the shelves, but that step never touches @book₁@, so
-- it collapses into an elision row that names @book₂@. An elided step is one
-- that leaves the failing value alone, which does not make it irrelevant.
-- The returning step's annotation names the stale shelf, so the reader can
-- work back to the cause.
module Gallery.Library (scenario) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Gallery.Scenario
import Hegel.Pool (Pool)
import Hegel.Pool qualified as Pool
import Hegel.Property (annotate, assert, forAll)
import Hegel.Report (FailureEvidence (..), Report, renderValue)
import Hegel.Stateful qualified as Stateful

scenario :: Scenario
scenario =
  Scenario
    { name = "library",
      title = "pool lineage: a loan slip outlives a re-shelving",
      settings = seeded,
      property = Stateful.run machine,
      ascii = True,
      attempts = 1,
      expect = pure . check
    }

data Library = Library
  { -- | Books on the shelves, available to lend.
    shelved :: Pool Int,
    -- | Books out on loan.
    lent :: Pool Int,
    shelves :: Int,
    -- | The books filed on each shelf.
    catalog :: Map Int [Int],
    -- | The shelf written on each lent book's slip.
    slips :: Map Int Int,
    -- | How many books the library owns, shelved or lent.
    owned :: Int
  }

-- | The shelf a book belongs on.
home :: Library -> Int -> Int
home lib book = book `mod` lib.shelves

file :: Int -> Int -> Library -> Library
file shelf book lib = lib {catalog = Map.insertWith (<>) shelf [book] lib.catalog}

unfile :: Int -> Int -> Library -> Library
unfile shelf book lib = lib {catalog = Map.adjust (filter (/= book)) shelf lib.catalog}

-- | Double the shelves and re-file every shelved book. Lent books are not on
-- any shelf, so their slips keep the old numbering.
grow :: Library -> Library
grow lib = foldr (\b l -> file (home l b) b l) grown (concat (Map.elems lib.catalog))
  where
    grown = lib {shelves = lib.shelves * 2, catalog = Map.empty}

acquire :: Stateful.Rule Library IO
acquire =
  Stateful.rule "acquire" \lib -> do
    let book = lib.owned + 1
        lib' = (if book > lib.shelves then grow lib else lib) {owned = book}
    Pool.add lib'.shelved book
    pure (file (home lib' book) book lib')

lend :: Stateful.Rule Library IO
lend =
  Stateful.rule "lend" \lib -> do
    book <- forAll (Pool.transfer lib.shelved lib.lent)
    let shelf = home lib book
    pure (unfile shelf book lib) {slips = Map.insert book shelf lib.slips}

-- | File a returned book on the shelf its slip names.
--
-- BUG: the slip's shelf is stale when the shelves doubled during the loan.
returnBook :: Stateful.Rule Library IO
returnBook =
  Stateful.rule "return" \lib -> do
    book <- forAll (Pool.transfer lib.lent lib.shelved)
    let slip = Map.findWithDefault 0 book lib.slips
        lib' = file slip book lib {slips = Map.delete book lib.slips}
        wanted = home lib' book
    Stateful.respond ("shelf " <> renderValue slip)
    annotate ("filed on shelf " <> renderValue slip <> "; the catalog looks on shelf " <> renderValue wanted <> " of " <> renderValue lib'.shelves)
    assert (book `elem` Map.findWithDefault [] wanted lib'.catalog) "the catalog finds a returned book"
    pure lib'

machine :: Stateful.Machine Library IO
machine =
  Stateful.Machine
    { initial = do
        shelved <- Pool.named "book"
        lent <- Pool.new
        pure Library {shelved, lent, shelves = 1, catalog = Map.empty, slips = Map.empty, owned = 0},
      rules = [acquire, lend, returnBook],
      invariants = [],
      stepCount = Stateful.defaultStepCount
    }

check :: Report -> [Text]
check report = case captured report of
  Right [evidence] ->
    ensureEqual "message" "the catalog finds a returned book" evidence.message
      <> ensureEqual
        "event log"
        [ LogRow False False "acquire book₁" Nothing,
          LogRow False False "lend book₁" Nothing,
          LogRow False True "1 step elided (book₂)" Nothing,
          LogRow True False "return book₁ → shelf 0" Nothing
        ]
        (logRows evidence)
  Right evidence -> ["expected one failure, got " <> renderValue (length evidence)]
  Left mismatch -> [mismatch]

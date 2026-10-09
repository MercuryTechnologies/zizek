-- | An HTTP handler whose upstream service is a 'Supply'-driven stub, behind
-- middleware that turns every exception into a 500.
module Http (spec) where

import Control.Exception (SomeException)
import Control.Exception qualified as E
import Control.Monad (forever)
import Data.Default.Class (def)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Gen qualified as Gen
import Hegel.HealthCheck (HealthCheck (..))
import Hegel.Property (assert, check)
import Hegel.Report (Abort (..), Report (..), Result (..), Stats (..))
import Hegel.Settings (Settings (..), defaultSettings)
import Hegel.Supply (Supply)
import Hegel.Supply qualified as Supply
import Support (isOk, viable)
import Test.Hspec

data Request = Request {userId :: Text}

data Response = Response {status :: Int, body :: Text}

type Handler = Request -> IO Response

-- | Answer every exception with a 500, as a web framework's top-level handler
-- does.
catchAll500 :: Handler -> Handler
catchAll500 handler req = handler req `E.catch` \(_ :: SomeException) -> pure (Response 500 "internal error")

-- | The upstream user service, reduced to the status code it answers with.
newtype Upstream = Upstream {fetch :: Text -> IO Int}

-- | An upstream that answers 503 as often as 200, so a run of 503s long
-- enough to exhaust the client's retries comes up within a few dozen cases.
flakyUpstream :: Supply -> Upstream
flakyUpstream supply = Upstream \uid ->
  Supply.draw supply ("GET /users/" <> uid) (Gen.frequency [(1, pure 200), (1, pure 503)])

-- | An upstream whose slow responses are out of scope for the property, so
-- drawing one discards the test case.
slowUpstream :: Supply -> Upstream
slowUpstream supply = Upstream \uid -> do
  _ <- Supply.draw supply ("latency " <> uid) do
    ms <- Gen.int & Gen.min 0 & Gen.max 1000 & Gen.build
    Gen.assume (ms < 500)
    pure ms
  pure 200

-- | An upstream that streams chunks until the engine stops the test case for
-- exceeding its choice budget.
chattyUpstream :: Supply -> Upstream
chattyUpstream supply = Upstream \uid -> forever do
  Supply.draw supply ("chunk " <> uid) (Gen.int & Gen.min 0 & Gen.max 1000 & Gen.build)

userHandler :: Upstream -> Handler
userHandler upstream req =
  upstream.fetch req.userId >>= \case
    200 -> pure (Response 200 ("user " <> req.userId))
    code -> pure (Response code "")

-- | Fetch a user, retrying a 503 up to twice more.
--
-- The bug: when the last retry also gets a 503, the client reports the empty
-- body as a success.
getUser :: Handler -> Text -> IO (Maybe Text)
getUser handler uid = go (3 :: Int)
  where
    go attempts = do
      resp <- handler (Request uid)
      case resp.status of
        200 -> pure (Just resp.body)
        503 | attempts > 1 -> go (attempts - 1)
        _ -> pure (Just resp.body)

users :: [Text]
users = ["alice", "bob", "carol"]

spec :: Spec
spec = describe "a stubbed upstream behind catch-all middleware" do
  it "finds and shrinks the retry bug to one user's three 503s" do
    drawn <- viable do
      results <- Supply.withSupply \supply ->
        traverse (getUser (catchAll500 (userHandler (flakyUpstream supply)))) users
      for_ results \result ->
        assert (maybe True (not . T.null) result) "the client reported an empty body as a success"
    drawn
      `shouldBe` [ "GET /users/alice=200",
                   "GET /users/bob=200",
                   "GET /users/carol=503",
                   "GET /users/carol=503",
                   "GET /users/carol=503"
                 ]

  it "discards a case even when the middleware swallows the signal" do
    report <- check def do
      statuses <- Supply.withSupply \supply ->
        traverse (\uid -> (.status) <$> catchAll500 (userHandler (slowUpstream supply)) (Request uid)) users
      assert (500 `notElem` statuses) "the middleware turned a discard into a 500"
    report.result `shouldSatisfy` isOk

  it "stops a case even when the middleware swallows the signal" do
    -- Suppressing only the initial-size check keeps the per-case choice
    -- limit, so every case overruns until the size health check aborts the
    -- run. A swallowed stop would instead count each case as passing.
    let settings = defaultSettings {suppressHealthCheck = Just [LargeInitialTestCase], testCases = Just 3}
    report <- check settings do
      statuses <- Supply.withSupply \supply ->
        traverse (\uid -> (.status) <$> catchAll500 (userHandler (chattyUpstream supply)) (Request uid)) users
      assert (500 `notElem` statuses) "the middleware turned a stop into a 500"
    case report.result of
      Aborted (UnhealthyInput msg) -> msg `shouldSatisfy` T.isInfixOf "TestCasesTooLarge"
      other -> expectationFailure ("expected a TestCasesTooLarge abort, got: " <> show other)
    (report.stats.valid, report.stats.failing) `shouldBe` (0, 0)

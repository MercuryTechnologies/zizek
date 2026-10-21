module DiffRendering (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Diff (diffShown, renderDiff)
import Test.Hspec

spec :: Spec
spec = describe "Hegel.Diff" $ do
  it "keeps the list separator on the added side of a multi-line removed item" $ do
    let wide :: Text
        wide =
          "Foo {alpha = Bar {xxxxxxxxxxxxxxx = 1, yyyyyyyyyyyyyyyyyyy = [1,2,3]},\
          \ beta = Baz 123456789 \"some long string value\" (Just 3),\
          \ gamma = [(1,2),(3,4)]}"
    fmap renderDiff (diffShown ("[1, " <> wide <> ", 3]") "[1, E, 3]")
      `shouldBe` Just
        ( T.intercalate
            "\n"
            [ "  [",
              "    1",
              "- , Foo",
              "-     { alpha =",
              "-         Bar { xxxxxxxxxxxxxxx = 1 , yyyyyyyyyyyyyyyyyyy = [ 1 , 2 , 3 ] }",
              "-     , beta = Baz 123456789 \"some long string value\" (Just 3)",
              "-     , gamma = [ ( 1 , 2 ) , ( 3 , 4 ) ]",
              "-     }",
              "+ , E",
              "  , 3",
              "  ]"
            ]
        )

module Arkham.GameSpec (spec) where

import TestImport

import Arkham.Classes.HasGame
import Test.HUnit.Base (assertFailure)

spec :: Spec
spec = describe "ToJSON" $ do
  it "is reversable" $ gameTest $ \_ -> do
    game <- getGame
    liftIO $ decode (encode game) == Just game `shouldBe` True

  -- passes at 1500000
  -- fails at 1000000
  -- ideally it would be 100000 or lower
  xit "should be fast enough" $ gameTestFromFile "example.json" $ \_ -> do
    mResult <- timeout 100000 $ chooseOnlyOption "Continue"
    liftIO $ maybe (assertFailure "timeout") (const $ pure ()) mResult

module Arkham.GameSpec (spec) where

import TestImport

import Arkham.Classes.HasGame

spec :: Spec
spec = describe "ToJSON" $ do
  it "is reversable" $ gameTest $ \_ -> do
    game <- getGame
    liftIO $ decode (encode game) == Just game `shouldBe` True

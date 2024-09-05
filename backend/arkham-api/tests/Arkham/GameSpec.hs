module Arkham.GameSpec (spec) where

import TestImport

import Arkham.Classes.HasGame

spec :: Spec
spec = describe "ToJSON" $ do
  it "is reversable" $ gameTest $ \_ -> do
    game <- getGame
    liftIO
      $ either (\err -> fail $ "Could not roundtrip: " <> err) (`shouldBe` game)
      $ eitherDecode' (encode game)

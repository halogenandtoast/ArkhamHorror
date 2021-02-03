module Arkham.Types.GameSpec
  ( spec
  ) where

import TestImport

spec :: Spec
spec = describe "ToJSON" $ do
  it "is reversable" $ do
    investigator <- testInvestigator "00000" id
    game <- flip toExternalGame mempty =<< newGame investigator [Done]
    decode (encode game) == Just game `shouldBe` True

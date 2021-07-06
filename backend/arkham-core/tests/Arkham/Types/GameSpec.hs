module Arkham.Types.GameSpec
  ( spec
  ) where

import TestImport

spec :: Spec
spec = describe "ToJSON" $ do
  it "is reversable" $ do
    investigator <- testInvestigator "00000" id
    game <- newGame investigator
    decode (encode game) == Just game `shouldBe` True

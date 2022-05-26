module Arkham.GameSpec
  ( spec
  ) where

import TestImport

spec :: Spec
spec = describe "ToJSON" $ do
  it "is reversable" $ do
    investigator <- testInvestigator id
    game <- newGame investigator
    decode (encode game) == Just game `shouldBe` True

module Arkham.Asset.Cards.LeoDeLuca1Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Leo De Luca1" $ do
  it "gives +1 action" . gameTest $ \self -> do
    self `putCardIntoPlay` Assets.leoDeLuca1
    self.additionalActions `shouldSatisfyM` ((== 1) . length)

module Arkham.Asset.Cards.StrayCatSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Stray Cat" $ do
  it "discard to automatically evade an enemy at your location" . gameTest $ \self -> do
    strayCat <- self `putAssetIntoPlay` Assets.strayCat
    enemy <- testEnemy
    location <- testLocation
    enemy `spawnAt` location
    self `moveTo` location
    [doEvade] <- self `getActionsFrom` strayCat
    self `useAbility` doEvade
    chooseTarget enemy
    assert enemy.exhausted
    self.engagedEnemies `shouldReturn` []
    asDefs self.discard `shouldReturn` [Assets.strayCat]

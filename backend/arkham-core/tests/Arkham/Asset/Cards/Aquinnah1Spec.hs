module Arkham.Asset.Cards.Aquinnah1Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Aquinnah (1)" $ do
  it "can redirect damage to another enemy at your location" . gameTest $ \self -> do
    enemy1 <- testEnemy & prop @"healthDamage" 2 & prop @"sanityDamage" 1
    enemy2 <- testEnemy & prop @"health" 3

    self `putCardIntoPlay` Assets.aquinnah1
    location <- testLocation
    enemy1 `spawnAt` location
    enemy2 `spawnAt` location
    self `moveTo` location

    run $ enemyAttack self enemy1
    useReaction
    chooseOnlyOption "damage enemy2"
    applyAllHorror
    self.horror `shouldReturn` 1
    self.damage `shouldReturn` 0
    enemy2.damage `shouldReturn` 2

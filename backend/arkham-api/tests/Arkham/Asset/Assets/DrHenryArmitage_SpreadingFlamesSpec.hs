{- HLINT ignore "Use camelCase" -}
module Arkham.Asset.Assets.DrHenryArmitage_SpreadingFlamesSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Dr. Henry Armitage (Spreading Flames)" $ do
  it "your first action each turn does not provoke attacks of opportunity" . gameTest $ \self -> do
    self `putCardIntoPlay` Assets.drHenryArmitage_SpreadingFlames
    loadDeck self [Assets.machete, Assets.machete, Assets.machete]
    enemy <- testEnemy & prop @"healthDamage" 2
    location <- testLocation
    enemy `spawnAt` location
    self `moveTo` location
    run $ engageEnemy self enemy

    -- First action of the turn: drawing a card does not provoke an attack of opportunity
    run $ drawCardsAction (toId self) self 1
    applyAllDamage
    self.damage `shouldReturn` 0

    -- A subsequent action that turn does provoke (the modifier only covers the first action)
    run $ drawCardsAction (toId self) self 1
    applyAllDamage
    self.damage `shouldReturn` 2

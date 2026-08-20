module Arkham.Treachery.Cards.BrethrenOfAsh.SmokeAndMirrors.ArcaneLockSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Treachery.CardDefs.BrethrenOfAsh.SmokeAndMirrors qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Arcane Lock" $ do
  it "provokes an attack of opportunity when a fast ability spends the action to leave"
    . gameTest
    $ \self -> do
      (here, there) <- testConnectedLocations id id
      arcaneLock <- genEncounterCard Treacheries.arcaneLock
      enemy <- testEnemy & prop @"healthDamage" 2
      pushAndRunAll
        [SetEncounterDeck (Deck [arcaneLock]), placedLocation here, placedLocation there]
      self `moveTo` here
      enemy `spawnAt` here
      run $ engageEnemy self enemy
      run $ drawEncounterCard self.id GameSource

      olivier <- self `putAssetIntoPlay` Assets.olivierBishopHaughtyArtCollector

      duringTurn self $ do
        [moveAway] <- self `getActionsFrom` olivier
        self `useAbility` moveAway
        chooseOnlyOption "resolve the attack of opportunity"
        applyAllDamage
        self.damage `shouldReturn` 2
        self.remainingActions `shouldReturn` 2
        self.location `shouldReturn` Just (toId there)

  it "provokes only one attack of opportunity when leaving with a move action" . gameTest $ \self -> do
    (here, there) <- testConnectedLocations id id
    arcaneLock <- genEncounterCard Treacheries.arcaneLock
    enemy <- testEnemy & prop @"healthDamage" 2
    pushAndRunAll
      [SetEncounterDeck (Deck [arcaneLock]), placedLocation here, placedLocation there]
    self `moveTo` here
    enemy `spawnAt` here
    run $ engageEnemy self enemy
    run $ drawEncounterCard self.id GameSource

    duringTurn self $ do
      [moveAway] <- self `getActionsFrom` there
      self `useAbility` moveAway
      chooseOnlyOption "resolve the attack of opportunity"
      applyAllDamage
      self.damage `shouldReturn` 2
      self.remainingActions `shouldReturn` 1
      self.location `shouldReturn` Just (toId there)

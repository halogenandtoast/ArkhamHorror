module Arkham.Investigator.Cards.BobJenkinsSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards (bobJenkins)
import Arkham.Matcher
import TestImport.New

spec :: Spec
spec = describe "Bob Jenkins" do
  -- The action Bob takes is the play, not the activation of an ability, so the
  -- played card's own text decides the attack of opportunity (#6045).
  it "playing Lupara (3) with his additional action does not provoke attacks of opportunity"
    . gameTestWith bobJenkins
    $ \self -> do
      loadDeck self [Assets.lupara3]
      drawCards self 1
      gainResources self 3
      location <- testLocation
      self `moveTo` location
      enemy <- testEnemy & prop @"healthDamage" 2
      enemy `spawnAt` location
      run $ engageEnemy self enemy

      duringTurn self do
        [bobAction] <- self `getActionsFrom` self
        self `useAbility` bobAction
        chooseOnlyOption "play Lupara (3)"
        applyAllDamage
        self.damage `shouldReturn` 0
        selectAny (assetIs Assets.lupara3) `shouldReturn` True

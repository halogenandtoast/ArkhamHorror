module Arkham.Skill.Cards.DaringSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Skills
import TestImport.New

spec :: Spec
spec = describe "Daring" $ do
  for_ [False, True] \returned -> do
    it
      ( if returned
          then "does not draw after returning to hand during an asset's test"
          else "draws when still committed at the end of an asset's test"
      ) . gameTest $ \self -> do
      cards <- testPlayerCards 1
      withProp @"deck" (Deck cards) self
      withProp @"combat" 5 self
      weapon <- self `putAssetIntoPlay` Assets.machete
      enemy <- testEnemy & prop @"health" 3 & prop @"fight" 1
      location <- testLocation
      daring <- genCard Skills.daring
      self `addToHand` daring
      enemy `spawnAt` location
      self `moveTo` location
      setChaosTokens [Zero]
      [attack] <- self `getActionsFrom` weapon
      self `useAbility` attack
      chooseTarget enemy
      commit daring
      startSkillTest
      when returned do
        skill <- selectJust $ SkillWithCardId daring.id
        run $ ReturnToHand self.id (SkillTarget skill)
      applyResults
      self.hand `shouldReturn` if returned then [toCard daring] else map toCard cards

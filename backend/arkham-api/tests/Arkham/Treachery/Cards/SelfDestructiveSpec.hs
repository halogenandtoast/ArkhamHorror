module Arkham.Treachery.Cards.SelfDestructiveSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Message qualified as Helpers
import Arkham.Investigator.Cards (rolandBanks)
import Arkham.Placement (Placement (InPlayArea, InThreatArea))
import Arkham.Treachery.CardDefs.Standalone qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Self-Destructive" do
  context "Forced - When you deal 1 or more damage to an enemy" do
    it "triggers for the investigator who dealt the damage" . gameTest $ \self -> do
      location <- testLocation
      self `moveTo` location
      selfDestructive <- genCard Treacheries.selfDestructive
      createSD <- Helpers.createTreacheryAt_ selfDestructive (InThreatArea self.id)
      machete <- genCard Assets.machete
      (macheteId, createMachete) <- Helpers.createAssetAt machete (InPlayArea self.id)
      runAll [createSD, createMachete]
      enemy <- testEnemy & prop @"health" 5
      enemy `spawnAt` location

      run $ InvestigatorDamageEnemy self.id enemy.id (AbilitySource (AssetSource macheteId) 1)
      chooseOnlyOption "Self-Destructive"
      applyAllDamage

      self.damage `shouldReturn` 1

    -- #5530: the trigger used to follow the *active* investigator rather than the
    -- attacker, because a weapon's damage source carried no attacker at all. Carson
    -- Sinclair's granted action leaves the active investigator pointing elsewhere,
    -- so a bystander's copy fired on someone else's attack.
    it "does not trigger for a co-located investigator who did not deal the damage" . gameTest $ \self -> do
      -- `self` holds Self-Destructive and is the active investigator; Roland swings.
      roland <- addInvestigator rolandBanks
      location <- testLocation
      self `moveTo` location
      roland `moveTo` location
      selfDestructive <- genCard Treacheries.selfDestructive
      (sdId, createSD) <- Helpers.createTreacheryAt selfDestructive (InThreatArea self.id)
      machete <- genCard Assets.machete
      (macheteId, createMachete) <- Helpers.createAssetAt machete (InPlayArea roland.id)
      runAll [createSD, createMachete]
      enemy <- testEnemy & prop @"health" 5
      enemy `spawnAt` location

      run $ InvestigatorDamageEnemy roland.id enemy.id (AbilitySource (AssetSource macheteId) 1)

      assertNoAbilityOf (TreacherySource sdId)
      applyAllDamage
      self.damage `shouldReturn` 0
      roland.damage `shouldReturn` 0

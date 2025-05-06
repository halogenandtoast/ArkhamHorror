module Arkham.Act.Cards.BreakingAndEntering (breakingAndEntering) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Enemy (spawnAt)
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers
import Arkham.Spawn

newtype BreakingAndEntering = BreakingAndEntering ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

breakingAndEntering :: ActCard BreakingAndEntering
breakingAndEntering = act (2, A) BreakingAndEntering Cards.breakingAndEntering Nothing

instance HasAbilities BreakingAndEntering where
  getAbilities (BreakingAndEntering x) =
    [mkAbility x 1 $ forced $ Enters #after You $ locationIs Cards.exhibitHallRestrictedHall]

instance RunMessage BreakingAndEntering where
  runMessage msg a@(BreakingAndEntering attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lead <- getLead
      investigators <- getInvestigators
      haroldWalsted <- fetchCard Assets.haroldWalsted
      chooseOneM lead do
        questionLabeled' "takeControlOfHaroldWalsted"
        questionLabeledCard haroldWalsted
        targets investigators (`takeControlOfSetAsideAsset` haroldWalsted)
      getHuntingHorror >>= \case
        Just eid -> do
          lid <- getRestrictedHall
          spawnAt eid Nothing (SpawnAtLocation lid)
          readyThis eid
        Nothing -> findEncounterCardIn lead attrs (cardIs Enemies.huntingHorror) [#deck, #discard, #void]
      advanceActDeck attrs
      pure a
    FoundEnemyInOutOfPlay zone _ (isTarget attrs -> True) eid -> do
      lid <- getRestrictedHall
      push $ EnemySpawnFromOutOfPlay zone Nothing lid eid
      pure a
    FoundEncounterCard _ (isTarget attrs -> True) ec -> do
      spawnEnemyAt_ ec =<< getRestrictedHall
      pure a
    _ -> BreakingAndEntering <$> liftRunMessage msg attrs

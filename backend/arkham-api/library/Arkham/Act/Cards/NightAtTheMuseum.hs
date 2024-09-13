module Arkham.Act.Cards.NightAtTheMuseum (NightAtTheMuseum (..), nightAtTheMuseum) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers hiding (advancedWithOther)
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Enemy (spawnAt)
import Arkham.Location.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers
import Arkham.Spawn

newtype NightAtTheMuseum = NightAtTheMuseum ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nightAtTheMuseum :: ActCard NightAtTheMuseum
nightAtTheMuseum = act (2, A) NightAtTheMuseum Cards.nightAtTheMuseum Nothing

instance HasAbilities NightAtTheMuseum where
  getAbilities (NightAtTheMuseum x) =
    [mkAbility x 1 $ forced $ Enters #after You $ locationIs Cards.exhibitHallRestrictedHall]

instance RunMessage NightAtTheMuseum where
  runMessage msg a@(NightAtTheMuseum attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      mHuntingHorror <- getHuntingHorror
      case mHuntingHorror of
        Just eid -> do
          spawnAt eid Nothing (SpawnAt $ LocationWithFullTitle "Exhibit Hall" "Restricted Hall")
          push $ Ready (EnemyTarget eid)
        Nothing ->
          push
            $ FindEncounterCard
              leadInvestigatorId
              (toTarget attrs)
              [FromEncounterDeck, FromEncounterDiscard, FromVoid]
              (cardIs Enemies.huntingHorror)
      advanceActDeck attrs
      pure a
    FoundEnemyInOutOfPlay VoidZone _ (isTarget attrs -> True) eid -> do
      lid <- getRestrictedHall
      pushAll [EnemySpawnFromOutOfPlay VoidZone Nothing lid eid]
      pure a
    FoundEncounterCard _ (isTarget attrs -> True) ec -> do
      lid <- getRestrictedHall
      pushAll [SpawnEnemyAt (EncounterCard ec) lid]
      pure a
    _ -> NightAtTheMuseum <$> liftRunMessage msg attrs

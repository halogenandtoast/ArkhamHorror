module Arkham.Treachery.Cards.GhostlyPresence (ghostlyPresence) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query
import Arkham.Location.Cards (siteOfTheSacrifice)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype GhostlyPresence = GhostlyPresence TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ghostlyPresence :: TreacheryCard GhostlyPresence
ghostlyPresence = treachery GhostlyPresence Cards.ghostlyPresence

instance RunMessage GhostlyPresence where
  runMessage msg t@(GhostlyPresence attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      selectOne (enemyIs Enemies.nahab) >>= \case
        Just nahab -> do
          readyThis nahab
          push $ HunterMove nahab
          doStep 1 msg
        Nothing -> findEncounterCard iid attrs Enemies.nahab
      pure t
    DoStep 1 (Revelation _iid (isSource attrs -> True)) -> do
      -- unlikely but perhaps nahab died from the hunter move
      selectForMaybeM (enemyIs Enemies.nahab) \nahab -> do
        lead <- getLead
        iids <- select $ InvestigatorAt $ locationWithEnemy nahab
        chooseOneAtATimeM lead $ targets iids $ initiateEnemyAttack nahab attrs
        whenAny (locationIs siteOfTheSacrifice <> locationWithEnemy nahab) $ placeDoom attrs nahab 1
      pure t
    FoundEncounterCard iid (isTarget attrs -> True) card -> do
      withLocationOf iid $ createEnemyAt_ card
      pure t
    _ -> GhostlyPresence <$> liftRunMessage msg attrs

module Arkham.Treachery.Cards.AerialPursuit (aerialPursuit) where

import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AerialPursuit = AerialPursuit TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aerialPursuit :: TreacheryCard AerialPursuit
aerialPursuit = treachery AerialPursuit Cards.aerialPursuit

instance RunMessage AerialPursuit where
  runMessage msg t@(AerialPursuit attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      select (NearestEnemyTo iid NonEliteEnemy) >>= \case
        [] -> pure ()
        [eid] -> moveEnemyAndAttack iid eid
        enemies -> chooseOneM iid $ for_ enemies $ \eid ->
          targeting eid $ moveEnemyAndAttack iid eid
      pure t
    _ -> AerialPursuit <$> liftRunMessage msg attrs

moveEnemyAndAttack :: ReverseQueue m => InvestigatorId -> EnemyId -> m ()
moveEnemyAndAttack iid eid = withLocationOf iid \destinationId -> do
  selectOne (locationWithEnemy eid) >>= traverse_ \locationId ->
    select (ClosestPathLocation locationId destinationId) >>= \case
      [] -> pure ()
      xs -> do
        chooseOneM iid do
          sequence_ [targeting x (push $ EnemyMove eid x) | x <- xs, x /= locationId]
        push $ EnemyAttackIfEngaged eid (Just iid)

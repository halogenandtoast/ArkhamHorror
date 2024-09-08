module Arkham.Treachery.Cards.HuntedDown (HuntedDown (..), huntedDown) where

import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HuntedDown = HuntedDown TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntedDown :: TreacheryCard HuntedDown
huntedDown = treachery HuntedDown Cards.huntedDown

instance RunMessage HuntedDown where
  runMessage msg t@(HuntedDown attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      select (UnengagedEnemy <> EnemyWithTrait Criminal) >>= \case
        [] -> gainSurge attrs
        enemiesToMove -> withLocationOf iid \destinationId -> do
          chooseOneAtATimeM iid $ for_ enemiesToMove $ \eid ->
            selectOne (locationWithEnemy eid) >>= traverse_ \locationId ->
              select (ClosestPathLocation locationId destinationId) >>= \case
                [] -> pure ()
                xs -> targeting eid do
                  chooseOneM iid do
                    sequence_ [targeting x (push $ EnemyMove eid x) | x <- xs, x /= locationId]
                  push $ EnemyAttackIfEngaged eid (Just iid)
      pure t
    _ -> HuntedDown <$> liftRunMessage msg attrs

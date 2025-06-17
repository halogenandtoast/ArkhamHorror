module Arkham.Treachery.Cards.ViciousAmbush (viciousAmbush) where

import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Message
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (Monster))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ViciousAmbush = ViciousAmbush TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

viciousAmbush :: TreacheryCard ViciousAmbush
viciousAmbush = treachery ViciousAmbush Cards.viciousAmbush

instance RunMessage ViciousAmbush where
  runMessage msg t@(ViciousAmbush attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      monsters <- select $ EnemyWithTrait Monster
      if null monsters
        then gainSurge attrs
        else withLocationOf iid \loc -> do
          enemies <- select $ NearestEnemyTo iid $ EnemyWithTrait Monster
          chooseOrRunOneM iid do
            targets enemies \enemy -> do
              ready enemy
              moveUntil enemy loc
              enemyEngageInvestigator enemy iid
              initiateEnemyAttack enemy attrs iid
      pure t
    _ -> ViciousAmbush <$> liftRunMessage msg attrs

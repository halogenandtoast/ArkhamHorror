module Arkham.Treachery.Cards.RaisingSuspicions (raisingSuspicions) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RaisingSuspicions = RaisingSuspicions TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

raisingSuspicions :: TreacheryCard RaisingSuspicions
raisingSuspicions = treachery RaisingSuspicions Cards.raisingSuspicions

instance RunMessage RaisingSuspicions where
  runMessage msg t@(RaisingSuspicions attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- select $ NearestEnemyTo iid $ EnemyWithoutDoom <> CanPlaceDoomOnEnemy
      if null enemies
        then gainSurge attrs
        else chooseTargetM iid enemies $ placeDoomOn attrs 1
      pure t
    _ -> RaisingSuspicions <$> liftRunMessage msg attrs

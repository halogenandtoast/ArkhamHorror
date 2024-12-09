module Arkham.Event.Events.OnTheLam (onTheLam) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype OnTheLam = OnTheLam EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheLam :: EventCard OnTheLam
onTheLam = event OnTheLam Cards.onTheLam

instance RunMessage OnTheLam where
  runMessage msg e@(OnTheLam attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      roundModifier eid iid (CannotBeAttackedBy NonEliteEnemy)
      pure e
    _ -> OnTheLam <$> liftRunMessage msg attrs

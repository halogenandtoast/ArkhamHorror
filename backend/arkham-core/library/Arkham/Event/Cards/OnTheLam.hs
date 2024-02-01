module Arkham.Event.Cards.OnTheLam where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype OnTheLam = OnTheLam EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

onTheLam :: EventCard OnTheLam
onTheLam = event OnTheLam Cards.onTheLam

instance RunMessage OnTheLam where
  runMessage msg e@(OnTheLam attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      push $ roundModifier eid iid (CannotBeAttackedBy NonEliteEnemy)
      pure e
    _ -> OnTheLam <$> runMessage msg attrs

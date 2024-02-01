module Arkham.Event.Cards.Dodge where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner

newtype Dodge = Dodge EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

dodge :: EventCard Dodge
dodge = event Dodge Cards.dodge

instance RunMessage Dodge where
  runMessage msg e@(Dodge attrs) = case msg of
    PlayThisEvent _ eid | attrs `is` eid -> do
      push $ CancelNext (toSource attrs) AttackMessage
      pure e
    _ -> Dodge <$> runMessage msg attrs

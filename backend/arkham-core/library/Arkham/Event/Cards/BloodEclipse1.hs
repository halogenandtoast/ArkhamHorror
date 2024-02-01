module Arkham.Event.Cards.BloodEclipse1 (
  bloodEclipse1,
  BloodEclipse1 (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers

newtype BloodEclipse1 = BloodEclipse1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

bloodEclipse1 :: EventCard BloodEclipse1
bloodEclipse1 = event BloodEclipse1 Cards.bloodEclipse1

instance RunMessage BloodEclipse1 where
  runMessage msg e@(BloodEclipse1 attrs) = case msg of
    PaidForCardCost iid card _ | toCardId card == toCardId attrs -> do
      pushAll
        [ skillTestModifiers attrs iid [DamageDealt 2, SkillModifier #willpower 2]
        , chooseFightEnemy iid attrs #willpower
        ]
      pure e
    _ -> BloodEclipse1 <$> runMessage msg attrs

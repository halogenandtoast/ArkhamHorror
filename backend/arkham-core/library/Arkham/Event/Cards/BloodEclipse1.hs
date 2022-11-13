module Arkham.Event.Cards.BloodEclipse1
  ( bloodEclipse1
  , BloodEclipse1(..)
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Message
import Arkham.SkillType
import Arkham.Target

newtype BloodEclipse1 = BloodEclipse1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodEclipse1 :: EventCard BloodEclipse1
bloodEclipse1 = event BloodEclipse1 Cards.bloodEclipse1

instance RunMessage BloodEclipse1 where
  runMessage msg e@(BloodEclipse1 attrs) = case msg of
    PaidForCardCost iid card _
      | toCardId card == toCardId attrs -> do
        pushAll
          [ skillTestModifiers
            (toSource attrs)
            (InvestigatorTarget iid)
            [DamageDealt 2, SkillModifier SkillWillpower 2]
          , ChooseFightEnemy
            iid
            (toSource attrs)
            Nothing
            SkillWillpower
            mempty
            False
          , Discard (toTarget attrs)
          ]
        pure e
    _ -> BloodEclipse1 <$> runMessage msg attrs

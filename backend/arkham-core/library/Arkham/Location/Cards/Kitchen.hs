module Arkham.Location.Cards.Kitchen (
  kitchen,
  Kitchen (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.ScenarioLogKey
import Arkham.SkillType

newtype Kitchen = Kitchen LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kitchen :: LocationCard Kitchen
kitchen = location Kitchen Cards.kitchen 2 (PerPlayer 1)

instance HasAbilities Kitchen where
  getAbilities (Kitchen attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility attrs 1 (Here <> NoCluesOnThis)
        $ ActionAbility []
        $ ActionCost 1
      | locationRevealed attrs
      ]

instance HasModifiersFor Kitchen where
  getModifiersFor (LocationTarget lid) (Kitchen attrs)
    | lid == toId attrs =
        pure $ toModifiers attrs [Blocked | not (locationRevealed attrs)]
  getModifiersFor _ _ = pure []

instance RunMessage Kitchen where
  runMessage msg l@(Kitchen attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          l
            <$ push
              ( beginSkillTest
                  iid
                  (attrs.ability 1)
                  (toTarget attrs)
                  SkillWillpower
                  (Fixed 2)
              )
    PassedSkillTest _ _ source SkillTestInitiatorTarget {} _ _
      | isAbilitySource attrs 1 source -> l <$ push (Remember SetAFireInTheKitchen)
    _ -> Kitchen <$> runMessage msg attrs

module Arkham.Location.Cards.Garden (
  garden,
  Garden (..),
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

newtype Garden = Garden LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

garden :: LocationCard Garden
garden = location Garden Cards.garden 3 (PerPlayer 1)

instance HasModifiersFor Garden where
  getModifiersFor (LocationTarget lid) (Garden attrs)
    | lid == toId attrs =
        pure $ toModifiers attrs [Blocked | not (locationRevealed attrs)]
  getModifiersFor _ _ = pure []

instance HasAbilities Garden where
  getAbilities (Garden attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility attrs 1 (Here <> NoCluesOnThis)
        $ ActionAbility []
        $ ActionCost 1
      | locationRevealed attrs
      ]

instance RunMessage Garden where
  runMessage msg l@(Garden attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          l
            <$ push
              ( beginSkillTest
                  iid
                  (attrs.ability 1)
                  (toTarget attrs)
                  SkillAgility
                  2
              )
    PassedSkillTest _ _ source SkillTestInitiatorTarget {} _ _
      | isAbilitySource attrs 1 source -> l <$ push (Remember DistractedTheGuards)
    _ -> Garden <$> runMessage msg attrs

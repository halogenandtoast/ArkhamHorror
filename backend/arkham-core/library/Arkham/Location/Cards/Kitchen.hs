module Arkham.Location.Cards.Kitchen
  ( kitchen
  , Kitchen(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Message
import Arkham.ScenarioLogKey
import Arkham.SkillType
import Arkham.Target

newtype Kitchen = Kitchen LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kitchen :: LocationCard Kitchen
kitchen = location Kitchen Cards.kitchen 2 (PerPlayer 1)

instance HasAbilities Kitchen where
  getAbilities (Kitchen attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 (Here <> NoCluesOnThis)
      $ ActionAbility Nothing
      $ ActionCost 1
    | locationRevealed attrs
    ]

instance HasModifiersFor Kitchen where
  getModifiersFor _ (LocationTarget lid) (Kitchen attrs) | lid == toId attrs =
    pure $ toModifiers attrs [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor _ _ _ = pure []

instance RunMessage Kitchen where
  runMessage msg l@(Kitchen attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (BeginSkillTest
        iid
        source
        (LocationTarget $ toId attrs)
        Nothing
        SkillWillpower
        2
      )
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> l <$ push (Remember SetAFireInTheKitchen)
    _ -> Kitchen <$> runMessage msg attrs

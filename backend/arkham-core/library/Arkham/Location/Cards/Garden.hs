module Arkham.Location.Cards.Garden
  ( garden
  , Garden(..)
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

newtype Garden = Garden LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

garden :: LocationCard Garden
garden = location Garden Cards.garden 3 (PerPlayer 1)

instance HasModifiersFor Garden where
  getModifiersFor (LocationTarget lid) (Garden attrs) | lid == toId attrs =
    pure $ toModifiers attrs [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor _ _ = pure []

instance HasAbilities Garden where
  getAbilities (Garden attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 (Here <> NoCluesOnThis)
      $ ActionAbility Nothing
      $ ActionCost 1
    | locationRevealed attrs
    ]

instance RunMessage Garden where
  runMessage msg l@(Garden attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (BeginSkillTest
        iid
        source
        (LocationTarget $ toId attrs)
        Nothing
        SkillAgility
        2
      )
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> l <$ push (Remember DistractedTheGuards)
    _ -> Garden <$> runMessage msg attrs

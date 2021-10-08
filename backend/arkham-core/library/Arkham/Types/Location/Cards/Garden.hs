module Arkham.Types.Location.Cards.Garden
  ( garden
  , Garden(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.ScenarioLogKey
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype Garden = Garden LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

garden :: LocationCard Garden
garden = location Garden Cards.garden 3 (PerPlayer 1) Plus [Diamond]

instance HasModifiersFor env Garden where
  getModifiersFor _ (LocationTarget lid) (Garden attrs) | lid == toId attrs =
    pure $ toModifiers attrs [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities Garden where
  getAbilities (Garden attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 (Here <> NoCluesOnThis)
      $ ActionAbility Nothing
      $ ActionCost 1
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage env Garden where
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

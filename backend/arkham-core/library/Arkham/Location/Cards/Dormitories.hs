module Arkham.Location.Cards.Dormitories where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( dormitories )
import Arkham.Location.Runner
import Arkham.Matcher hiding ( FastPlayerWindow )
import Arkham.Message
import Arkham.Resolution

newtype Dormitories = Dormitories LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dormitories :: LocationCard Dormitories
dormitories = location Dormitories Cards.dormitories 1 (PerPlayer 3)

instance HasModifiersFor Dormitories where
  getModifiersFor target (Dormitories attrs) | isTarget attrs target =
    pure $ toModifiers attrs [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor _ _ = pure []

instance HasAbilities Dormitories where
  getAbilities (Dormitories attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility attrs 1 Here
          $ Objective
          $ FastAbility
          $ GroupClueCost (PerPlayer 3) (LocationWithTitle "Dormitories")
        ]

instance RunMessage Dormitories where
  runMessage msg l@(Dormitories attrs) = case msg of
    UseCardAbility _iid source _ 1 _
      | isSource attrs source && locationRevealed attrs -> l
      <$ push (ScenarioResolution $ Resolution 2)
    _ -> Dormitories <$> runMessage msg attrs

module Arkham.Types.Location.Cards.Dormitories where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (dormitories)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher hiding (FastPlayerWindow)
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Resolution

newtype Dormitories = Dormitories LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dormitories :: LocationCard Dormitories
dormitories =
  location Dormitories Cards.dormitories 1 (PerPlayer 3) Equals [Diamond]

instance HasModifiersFor env Dormitories where
  getModifiersFor _ target (Dormitories attrs) | isTarget attrs target =
    pure $ toModifiers attrs [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities env Dormitories where
  getAbilities iid window (Dormitories attrs) =
    withBaseAbilities iid window attrs $ pure
      [ restrictedAbility attrs 1 Here
        $ FastAbility
        $ GroupClueCost (PerPlayer 3)
        $ Just (LocationWithTitle "Dormitories")
      ]

instance LocationRunner env => RunMessage env Dormitories where
  runMessage msg l@(Dormitories attrs) = case msg of
    UseCardAbility _iid source _ 1 _
      | isSource attrs source && locationRevealed attrs -> l
      <$ push (ScenarioResolution $ Resolution 2)
    _ -> Dormitories <$> runMessage msg attrs

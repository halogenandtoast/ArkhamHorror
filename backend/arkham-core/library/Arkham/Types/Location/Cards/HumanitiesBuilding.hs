module Arkham.Types.Location.Cards.HumanitiesBuilding where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (humanitiesBuilding)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Timing qualified as Timing

newtype HumanitiesBuilding = HumanitiesBuilding LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

humanitiesBuilding :: LocationCard HumanitiesBuilding
humanitiesBuilding = location
  HumanitiesBuilding
  Cards.humanitiesBuilding
  3
  (PerPlayer 2)
  Square
  [Plus, Triangle]

instance HasAbilities HumanitiesBuilding where
  getAbilities (HumanitiesBuilding attrs) =
    withBaseAbilities attrs $
      [ restrictedAbility attrs 1 Here $ ForcedAbility $ TurnEnds
          Timing.When
          You
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env HumanitiesBuilding where
  runMessage msg l@(HumanitiesBuilding attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      horror <- unHorrorCount <$> getCount iid
      l <$ when (horror > 0) (push $ DiscardTopOfDeck iid horror Nothing)
    _ -> HumanitiesBuilding <$> runMessage msg attrs

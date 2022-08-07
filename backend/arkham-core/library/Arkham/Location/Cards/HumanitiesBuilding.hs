module Arkham.Location.Cards.HumanitiesBuilding where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Cards ( humanitiesBuilding )
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype HumanitiesBuilding = HumanitiesBuilding LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

humanitiesBuilding :: LocationCard HumanitiesBuilding
humanitiesBuilding =
  location HumanitiesBuilding Cards.humanitiesBuilding 3 (PerPlayer 2)

instance HasAbilities HumanitiesBuilding where
  getAbilities (HumanitiesBuilding attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility attrs 1 Here $ ForcedAbility $ TurnEnds
            Timing.When
            You
        | locationRevealed attrs
        ]

instance RunMessage HumanitiesBuilding where
  runMessage msg l@(HumanitiesBuilding attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      horror <- field InvestigatorHorror iid
      l <$ when (horror > 0) (push $ DiscardTopOfDeck iid horror Nothing)
    _ -> HumanitiesBuilding <$> runMessage msg attrs

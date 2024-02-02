module Arkham.Location.Cards.CloverClubBar (
  cloverClubBar,
  CloverClubBar (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards (cloverClubBar)
import Arkham.Location.Runner
import Arkham.Name
import Arkham.Projection
import Arkham.ScenarioLogKey

newtype CloverClubBar = CloverClubBar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

cloverClubBar :: LocationCard CloverClubBar
cloverClubBar = location CloverClubBar Cards.cloverClubBar 3 (Static 0)

instance HasAbilities CloverClubBar where
  getAbilities (CloverClubBar attrs) =
    withRevealedAbilities attrs
      $ [ limitedAbility (PlayerLimit PerGame 1)
            $ restrictedAbility attrs 1 (OnAct 1 <> Here)
            $ ActionAbility []
            $ Costs [ActionCost 1, ResourceCost 2]
        ]

instance RunMessage CloverClubBar where
  runMessage msg l@(CloverClubBar attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      drawing <- drawCards iid (toAbilitySource attrs 1) 2
      name <- field InvestigatorName iid
      pushAll
        [GainClues iid (toAbilitySource attrs 1) 2, drawing, Remember $ HadADrink $ labeled name iid]
      pure l
    _ -> CloverClubBar <$> runMessage msg attrs

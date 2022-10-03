module Arkham.Location.Cards.LabyrinthOfBones
  ( labyrinthOfBones
  , LabyrinthOfBones(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message hiding ( RevealLocation )
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers
import Arkham.Timing qualified as Timing

newtype LabyrinthOfBones = LabyrinthOfBones LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

labyrinthOfBones :: LocationCard LabyrinthOfBones
labyrinthOfBones = locationWith
  LabyrinthOfBones
  Cards.labyrinthOfBones
  2
  (PerPlayer 2)
  ((connectsToL .~ adjacentLocations)
  . (costToEnterUnrevealedL
    .~ Costs [ActionCost 1, GroupClueCost (PerPlayer 1) YourLocation]
    )
  )

instance HasAbilities LabyrinthOfBones where
  getAbilities (LabyrinthOfBones attrs) = withBaseAbilities
    attrs
    [ restrictedAbility
        attrs
        1
        (AnyCriterion
          [ Negate
              (LocationExists
              $ LocationInDirection dir (LocationWithId $ toId attrs)
              )
          | dir <- [Above, Below, RightOf]
          ]
        )
      $ ForcedAbility
      $ RevealLocation Timing.When Anyone
      $ LocationWithId
      $ toId attrs
    | locationRevealed attrs
    ]

instance RunMessage LabyrinthOfBones where
  runMessage msg l@(LabyrinthOfBones attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      n <- countM (directionEmpty attrs) [Above, Below, RightOf]
      push (DrawFromScenarioDeck iid CatacombsDeck (toTarget attrs) n)
      pure l
    DrewFromScenarioDeck _ _ (isTarget attrs -> True) cards -> do
      placements <- mapMaybeM (toMaybePlacement attrs) [Above, Below, RightOf]
      pushAll $ concat $ zipWith ($) placements cards
      pure l
    _ -> LabyrinthOfBones <$> runMessage msg attrs

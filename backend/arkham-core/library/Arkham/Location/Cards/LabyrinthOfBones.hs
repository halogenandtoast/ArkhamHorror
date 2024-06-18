module Arkham.Location.Cards.LabyrinthOfBones (labyrinthOfBones, LabyrinthOfBones (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers

newtype LabyrinthOfBones = LabyrinthOfBones LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

labyrinthOfBones :: LocationCard LabyrinthOfBones
labyrinthOfBones =
  locationWith LabyrinthOfBones Cards.labyrinthOfBones 2 (PerPlayer 2)
    $ (connectsToL .~ adjacentLocations)
    . ( costToEnterUnrevealedL
          .~ Costs [ActionCost 1, GroupClueCost (PerPlayer 1) YourLocation]
      )

instance HasAbilities LabyrinthOfBones where
  getAbilities (LabyrinthOfBones attrs) =
    extendRevealed
      attrs
      [ restrictedAbility
          attrs
          1
          (oneOf [notExists $ LocationInDirection dir (be attrs) | dir <- [Above, Below, RightOf]])
          $ forced
          $ RevealLocation #when Anyone (be attrs)
      ]

instance RunMessage LabyrinthOfBones where
  runMessage msg l@(LabyrinthOfBones attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      n <- countM (directionEmpty attrs) [Above, Below, RightOf]
      push $ DrawCards iid $ targetCardDraw attrs CatacombsDeck n
      pure l
    DrewCards _ drewCards | maybe False (isTarget attrs) drewCards.target -> do
      placeDrawnLocations attrs drewCards.cards [Above, Below, RightOf]
      pure l
    _ -> LabyrinthOfBones <$> runMessage msg attrs

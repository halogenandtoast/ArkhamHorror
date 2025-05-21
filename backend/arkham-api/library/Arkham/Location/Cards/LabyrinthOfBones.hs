module Arkham.Location.Cards.LabyrinthOfBones (labyrinthOfBones) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers

newtype LabyrinthOfBones = LabyrinthOfBones LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

labyrinthOfBones :: LocationCard LabyrinthOfBones
labyrinthOfBones =
  locationWith LabyrinthOfBones Cards.labyrinthOfBones 2 (PerPlayer 2)
    $ connectsToAdjacent
    . (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance HasAbilities LabyrinthOfBones where
  getAbilities (LabyrinthOfBones a) =
    extendRevealed1 a
      $ restricted a 1 (oneOf [notExists $ LocationInDirection dir (be a) | dir <- [Above, Below, RightOf]])
      $ forced
      $ RevealLocation #when Anyone (be a)

instance RunMessage LabyrinthOfBones where
  runMessage msg l@(LabyrinthOfBones attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- countM (directionEmpty attrs) [Above, Below, RightOf]
      push $ DrawCards iid $ targetCardDraw attrs CatacombsDeck n
      pure l
    DrewCards _ drewCards | maybe False (isTarget attrs) drewCards.target -> do
      placeDrawnLocations attrs drewCards.cards [Above, Below, RightOf]
      pure l
    _ -> LabyrinthOfBones <$> liftRunMessage msg attrs

module Arkham.Location.Cards.RightTurnA (rightTurnA) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.WrittenInRock.Helpers

newtype RightTurnA = RightTurnA LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rightTurnA :: LocationCard RightTurnA
rightTurnA = location RightTurnA Cards.rightTurnA 2 (PerPlayer 1)

instance HasAbilities RightTurnA where
  getAbilities (RightTurnA a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> HasAdjacentLocations LocationCanBeSwapped) doubleActionAbility

instance RunMessage RightTurnA where
  runMessage msg l@(RightTurnA attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select LocationCanBeSwapped
      chooseTargetM iid locations $ handleTarget iid (attrs.ability 1)
      pure l
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (LocationTarget lid) -> do
      pos <- fieldJust LocationPosition lid
      locations <-
        select $ mapOneOf LocationInPosition (adjacentPositions pos) <> LocationCanBeSwapped
      chooseTargetM iid locations (swapLocations lid)
      pure l
    _ -> RightTurnA <$> liftRunMessage msg attrs

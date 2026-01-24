module Arkham.Location.Cards.LeftTurnA (leftTurnA) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.WrittenInRock.Helpers

newtype LeftTurnA = LeftTurnA LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leftTurnA :: LocationCard LeftTurnA
leftTurnA = location LeftTurnA Cards.leftTurnA 2 (PerPlayer 1)

instance HasAbilities LeftTurnA where
  getAbilities (LeftTurnA a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> HasAdjacentLocations LocationCanBeSwapped) doubleActionAbility

instance RunMessage LeftTurnA where
  runMessage msg l@(LeftTurnA attrs) = runQueueT $ case msg of
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
    _ -> LeftTurnA <$> liftRunMessage msg attrs

module Arkham.Location.Cards.RightTurnB (rightTurnB) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.WrittenInRock.Helpers

newtype RightTurnB = RightTurnB LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rightTurnB :: LocationCard RightTurnB
rightTurnB = location RightTurnB Cards.rightTurnB 2 (PerPlayer 1)

instance HasAbilities RightTurnB where
  getAbilities (RightTurnB a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> exists LocationCanBeSlid)
      $ actionAbilityWithCost (ResourceCost 2)

instance RunMessage RightTurnB where
  runMessage msg (RightTurnB attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select LocationCanBeSlid
      chooseTargetM iid locations $ handleTarget iid (attrs.ability 1)
      pure l
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (LocationTarget lid) -> do
      slideLocations <-
        matches lid LocationCanBeSlid >>= \case
          False -> pure []
          True -> getEmptyPositions lid
      chooseOneM iid do
        for_ slideLocations \newPos ->
          gridLabeled (gridLabel newPos) $ push $ PlaceGrid (GridLocation newPos lid)
      pure l
    _ -> RightTurnB <$> liftRunMessage msg attrs

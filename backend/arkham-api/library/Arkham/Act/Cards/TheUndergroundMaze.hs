module Arkham.Act.Cards.TheUndergroundMaze (theUndergroundMaze) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.WrittenInRock.Helpers

newtype TheUndergroundMaze = TheUndergroundMaze ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theUndergroundMaze :: ActCard TheUndergroundMaze
theUndergroundMaze = act (2, A) TheUndergroundMaze Cards.theUndergroundMaze Nothing

instance HasAbilities TheUndergroundMaze where
  getAbilities (TheUndergroundMaze a) =
    extend
      a
      [ restricted
          a
          1
          (DuringTurn You <> oneOf [HasAdjacentLocations LocationCanBeSwapped, exists LocationCanBeSlid])
          $ FastAbility (GroupClueCost (PerPlayer 1) Anywhere)
      ]

instance RunMessage TheUndergroundMaze where
  runMessage msg a@(TheUndergroundMaze attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ oneOf [LocationCanBeSwapped, LocationCanBeSlid]
      chooseTargetM iid locations $ handleTarget iid (attrs.ability 1)
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (LocationTarget lid) -> do
      pos <- fieldJust LocationPosition lid
      swappable <- select $ mapOneOf LocationInPosition (adjacentPositions pos) <> LocationCanBeSwapped
      slideLocations <- matches lid LocationCanBeSlid >>= \case
        False -> pure []
        True -> getEmptyPositions lid
      chooseOneM iid do
        targets swappable (swapLocations lid)
        for_ slideLocations \newPos ->
          gridLabeled (gridLabel newPos) $ push $ PlaceGrid (GridLocation newPos lid)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheUndergroundMaze <$> liftRunMessage msg attrs

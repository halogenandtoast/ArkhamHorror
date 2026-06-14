module Arkham.Location.Cards.WestAntechamber (westAntechamber) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (LocationPosition))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Projection
import Arkham.Trait (Trait (Lift))

newtype WestAntechamber = WestAntechamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

westAntechamber :: LocationCard WestAntechamber
westAntechamber = location WestAntechamber Cards.westAntechamber 3 (Static 1)

instance HasAbilities WestAntechamber where
  getAbilities (WestAntechamber a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> DuringTurn You <> thisExists a (not_ FloodedLocation))
          $ FastAbility (GroupClueCost (PerPlayer 1) (be a))
      , restricted a 2 (Here <> DuringTurn You <> thisExists a FloodedLocation)
          $ FastAbility (GroupClueCost (PerPlayer 2) (be a))
      ]

-- | Slide the Great Lift down once: move the Great Lift location down one level
-- (toward level 1 = lower grid row), carrying all its cards/tokens/investigators
-- (they stay attached to the same LocationId, so @PlaceGrid@ preserves them).
-- The lift cannot slide below level 1 (row 0).
slideGreatLiftDown :: ReverseQueue m => m ()
slideGreatLiftDown = do
  selectOne (LocationWithTrait Lift) >>= traverse_ \greatLift -> do
    field LocationPosition greatLift >>= traverse_ \pos ->
      when (positionRow pos > 0) do
        push $ PlaceGrid (GridLocation (updatePosition pos GridDown) greatLift)

instance RunMessage WestAntechamber where
  runMessage msg l@(WestAntechamber attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) n | n `elem` [1, 2] -> do
      slideGreatLiftDown
      pure l
    _ -> WestAntechamber <$> liftRunMessage msg attrs

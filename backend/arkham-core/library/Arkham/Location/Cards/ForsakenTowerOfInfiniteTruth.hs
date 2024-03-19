module Arkham.Location.Cards.ForsakenTowerOfInfiniteTruth
  ( forsakenTowerOfInfiniteTruth
  , ForsakenTowerOfInfiniteTruth(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ForsakenTowerOfInfiniteTruth = ForsakenTowerOfInfiniteTruth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forsakenTowerOfInfiniteTruth :: LocationCard ForsakenTowerOfInfiniteTruth
forsakenTowerOfInfiniteTruth = location ForsakenTowerOfInfiniteTruth Cards.forsakenTowerOfInfiniteTruth 2 (PerPlayer 1)

instance HasAbilities ForsakenTowerOfInfiniteTruth where
  getAbilities (ForsakenTowerOfInfiniteTruth attrs) =
    extendRevealed attrs []

instance RunMessage ForsakenTowerOfInfiniteTruth where
  runMessage msg (ForsakenTowerOfInfiniteTruth attrs) = runQueueT $ case msg of
    _ -> ForsakenTowerOfInfiniteTruth <$> lift (runMessage msg attrs)

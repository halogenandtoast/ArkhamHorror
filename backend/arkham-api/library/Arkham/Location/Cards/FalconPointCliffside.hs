module Arkham.Location.Cards.FalconPointCliffside
  ( falconPointCliffside
  , FalconPointCliffside(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype FalconPointCliffside = FalconPointCliffside LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falconPointCliffside :: LocationCard FalconPointCliffside
falconPointCliffside = location FalconPointCliffside Cards.falconPointCliffside 0 (Static 0)

instance HasAbilities FalconPointCliffside where
  getAbilities (FalconPointCliffside attrs) =
    extendRevealed attrs []

instance RunMessage FalconPointCliffside where
  runMessage msg (FalconPointCliffside attrs) = runQueueT $ case msg of
    _ -> FalconPointCliffside <$> liftRunMessage msg attrs

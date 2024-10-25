module Arkham.Location.Cards.HoldingCells
  ( holdingCells
  , HoldingCells(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype HoldingCells = HoldingCells LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

holdingCells :: LocationCard HoldingCells
holdingCells = location HoldingCells Cards.holdingCells 0 (Static 0)

instance HasAbilities HoldingCells where
  getAbilities (HoldingCells attrs) =
    extendRevealed attrs []

instance RunMessage HoldingCells where
  runMessage msg (HoldingCells attrs) = runQueueT $ case msg of
    _ -> HoldingCells <$> liftRunMessage msg attrs

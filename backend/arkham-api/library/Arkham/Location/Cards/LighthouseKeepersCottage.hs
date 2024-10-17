module Arkham.Location.Cards.LighthouseKeepersCottage
  ( lighthouseKeepersCottage
  , LighthouseKeepersCottage(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LighthouseKeepersCottage = LighthouseKeepersCottage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lighthouseKeepersCottage :: LocationCard LighthouseKeepersCottage
lighthouseKeepersCottage = location LighthouseKeepersCottage Cards.lighthouseKeepersCottage 0 (Static 0)

instance HasAbilities LighthouseKeepersCottage where
  getAbilities (LighthouseKeepersCottage attrs) =
    extendRevealed attrs []

instance RunMessage LighthouseKeepersCottage where
  runMessage msg (LighthouseKeepersCottage attrs) = runQueueT $ case msg of
    _ -> LighthouseKeepersCottage <$> liftRunMessage msg attrs

module Arkham.Location.Cards.LighthouseKeepersCottage (
  lighthouseKeepersCottage,
  LighthouseKeepersCottage (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.ALightInTheFog.Helpers.Location

newtype LighthouseKeepersCottage = LighthouseKeepersCottage LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lighthouseKeepersCottage :: LocationCard LighthouseKeepersCottage
lighthouseKeepersCottage = location LighthouseKeepersCottage Cards.lighthouseKeepersCottage 4 (PerPlayer 2)

instance HasModifiersFor LighthouseKeepersCottage where
  getModifiersFor target (LighthouseKeepersCottage attrs) = preventDrawConnections target attrs

instance HasAbilities LighthouseKeepersCottage where
  getAbilities (LighthouseKeepersCottage attrs) =
    extendRevealed attrs []

instance RunMessage LighthouseKeepersCottage where
  runMessage msg l@(LighthouseKeepersCottage attrs) = runQueueT $ case msg of
    PlaceGrid (GridLocation pos lid) | lid == attrs.id -> setConnectedInRow pos l
    _ -> LighthouseKeepersCottage <$> liftRunMessage msg attrs

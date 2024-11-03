module Arkham.Location.Cards.LighthouseStairwell (lighthouseStairwell, LighthouseStairwell (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.ALightInTheFog.Helpers.Location

newtype LighthouseStairwell = LighthouseStairwell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lighthouseStairwell :: LocationCard LighthouseStairwell
lighthouseStairwell = location LighthouseStairwell Cards.lighthouseStairwell 3 (PerPlayer 1)

instance HasAbilities LighthouseStairwell where
  getAbilities (LighthouseStairwell attrs) =
    extendRevealed attrs []

instance RunMessage LighthouseStairwell where
  runMessage msg l@(LighthouseStairwell attrs) = runQueueT $ case msg of
    PlaceGrid (GridLocation pos lid) | lid == attrs.id -> setConnectedInRow pos l
    _ -> LighthouseStairwell <$> liftRunMessage msg attrs

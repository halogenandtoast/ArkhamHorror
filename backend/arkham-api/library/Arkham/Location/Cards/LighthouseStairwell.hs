module Arkham.Location.Cards.LighthouseStairwell
  ( lighthouseStairwell
  , LighthouseStairwell(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LighthouseStairwell = LighthouseStairwell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lighthouseStairwell :: LocationCard LighthouseStairwell
lighthouseStairwell = location LighthouseStairwell Cards.lighthouseStairwell 0 (Static 0)

instance HasAbilities LighthouseStairwell where
  getAbilities (LighthouseStairwell attrs) =
    extendRevealed attrs []

instance RunMessage LighthouseStairwell where
  runMessage msg (LighthouseStairwell attrs) = runQueueT $ case msg of
    _ -> LighthouseStairwell <$> liftRunMessage msg attrs

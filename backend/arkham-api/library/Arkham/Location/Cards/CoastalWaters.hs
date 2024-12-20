module Arkham.Location.Cards.CoastalWaters (coastalWaters) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CoastalWaters = CoastalWaters LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coastalWaters :: LocationCard CoastalWaters
coastalWaters = location CoastalWaters Cards.coastalWaters 2 (PerPlayer 4)

instance HasAbilities CoastalWaters where
  getAbilities (CoastalWaters attrs) =
    extendRevealed attrs []

instance RunMessage CoastalWaters where
  runMessage msg (CoastalWaters attrs) = runQueueT $ case msg of
    _ -> CoastalWaters <$> liftRunMessage msg attrs

module Arkham.Location.Cards.PalacioErrazuriz (palacioErrazuriz) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype PalacioErrazuriz = PalacioErrazuriz LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

palacioErrazuriz :: LocationCard PalacioErrazuriz
palacioErrazuriz = setLabel "g" $ location PalacioErrazuriz Cards.palacioErrazuriz 2 (PerPlayer 1)

instance HasAbilities PalacioErrazuriz where
  getAbilities (PalacioErrazuriz attrs) =
    extendRevealed attrs []

instance RunMessage PalacioErrazuriz where
  runMessage msg (PalacioErrazuriz attrs) = runQueueT $ case msg of
    _ -> PalacioErrazuriz <$> liftRunMessage msg attrs

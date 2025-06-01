module Arkham.Location.Cards.SnakePit (snakePit) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SnakePit = SnakePit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snakePit :: LocationCard SnakePit
snakePit = location SnakePit Cards.snakePit 1 (PerPlayer 1)

instance HasAbilities SnakePit where
  getAbilities (SnakePit attrs) =
    extendRevealed attrs []

instance RunMessage SnakePit where
  runMessage msg (SnakePit attrs) = runQueueT $ case msg of
    _ -> SnakePit <$> liftRunMessage msg attrs

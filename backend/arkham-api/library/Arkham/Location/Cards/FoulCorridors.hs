module Arkham.Location.Cards.FoulCorridors
  ( foulCorridors
  , FoulCorridors(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype FoulCorridors = FoulCorridors LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foulCorridors :: LocationCard FoulCorridors
foulCorridors = location FoulCorridors Cards.foulCorridors 2 (Static 0)

instance HasAbilities FoulCorridors where
  getAbilities (FoulCorridors attrs) =
    extendRevealed attrs []

instance RunMessage FoulCorridors where
  runMessage msg (FoulCorridors attrs) = runQueueT $ case msg of
    _ -> FoulCorridors <$> liftRunMessage msg attrs

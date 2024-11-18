module Arkham.Location.Cards.FrigidCave (frigidCave, FrigidCave (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype FrigidCave = FrigidCave LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frigidCave :: LocationCard FrigidCave
frigidCave = symbolLabel $ location FrigidCave Cards.frigidCave 0 (Static 0)

instance HasAbilities FrigidCave where
  getAbilities (FrigidCave attrs) =
    extendRevealed attrs []

instance RunMessage FrigidCave where
  runMessage msg (FrigidCave attrs) = runQueueT $ case msg of
    _ -> FrigidCave <$> liftRunMessage msg attrs

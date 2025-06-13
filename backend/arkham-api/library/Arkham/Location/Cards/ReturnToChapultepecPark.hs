module Arkham.Location.Cards.ReturnToChapultepecPark (returnToChapultepecPark) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ReturnToChapultepecPark = ReturnToChapultepecPark LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToChapultepecPark :: LocationCard ReturnToChapultepecPark
returnToChapultepecPark = symbolLabel $ location ReturnToChapultepecPark Cards.returnToChapultepecPark 2 (Static 0)

instance HasAbilities ReturnToChapultepecPark where
  getAbilities (ReturnToChapultepecPark attrs) =
    extendRevealed attrs []

instance RunMessage ReturnToChapultepecPark where
  runMessage msg (ReturnToChapultepecPark attrs) = runQueueT $ case msg of
    _ -> ReturnToChapultepecPark <$> liftRunMessage msg attrs

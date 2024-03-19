module Arkham.Location.Cards.ForsakenTowerOfPrimevalLight
  ( forsakenTowerOfPrimevalLight
  , ForsakenTowerOfPrimevalLight(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ForsakenTowerOfPrimevalLight = ForsakenTowerOfPrimevalLight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forsakenTowerOfPrimevalLight :: LocationCard ForsakenTowerOfPrimevalLight
forsakenTowerOfPrimevalLight = location ForsakenTowerOfPrimevalLight Cards.forsakenTowerOfPrimevalLight 4 (PerPlayer 1)

instance HasAbilities ForsakenTowerOfPrimevalLight where
  getAbilities (ForsakenTowerOfPrimevalLight attrs) =
    extendRevealed attrs []

instance RunMessage ForsakenTowerOfPrimevalLight where
  runMessage msg (ForsakenTowerOfPrimevalLight attrs) = runQueueT $ case msg of
    _ -> ForsakenTowerOfPrimevalLight <$> lift (runMessage msg attrs)

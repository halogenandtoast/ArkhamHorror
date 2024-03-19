module Arkham.Location.Cards.ForsakenTowerOfLifeAndDeath
  ( forsakenTowerOfLifeAndDeath
  , ForsakenTowerOfLifeAndDeath(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ForsakenTowerOfLifeAndDeath = ForsakenTowerOfLifeAndDeath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forsakenTowerOfLifeAndDeath :: LocationCard ForsakenTowerOfLifeAndDeath
forsakenTowerOfLifeAndDeath = location ForsakenTowerOfLifeAndDeath Cards.forsakenTowerOfLifeAndDeath 2 (PerPlayer 1)

instance HasAbilities ForsakenTowerOfLifeAndDeath where
  getAbilities (ForsakenTowerOfLifeAndDeath attrs) =
    extendRevealed attrs []

instance RunMessage ForsakenTowerOfLifeAndDeath where
  runMessage msg (ForsakenTowerOfLifeAndDeath attrs) = runQueueT $ case msg of
    _ -> ForsakenTowerOfLifeAndDeath <$> lift (runMessage msg attrs)

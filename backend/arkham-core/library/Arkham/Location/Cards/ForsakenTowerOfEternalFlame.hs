module Arkham.Location.Cards.ForsakenTowerOfEternalFlame
  ( forsakenTowerOfEternalFlame
  , ForsakenTowerOfEternalFlame(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ForsakenTowerOfEternalFlame = ForsakenTowerOfEternalFlame LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forsakenTowerOfEternalFlame :: LocationCard ForsakenTowerOfEternalFlame
forsakenTowerOfEternalFlame = location ForsakenTowerOfEternalFlame Cards.forsakenTowerOfEternalFlame 3 (PerPlayer 1)

instance HasAbilities ForsakenTowerOfEternalFlame where
  getAbilities (ForsakenTowerOfEternalFlame attrs) =
    extendRevealed attrs []

instance RunMessage ForsakenTowerOfEternalFlame where
  runMessage msg (ForsakenTowerOfEternalFlame attrs) = runQueueT $ case msg of
    _ -> ForsakenTowerOfEternalFlame <$> lift (runMessage msg attrs)

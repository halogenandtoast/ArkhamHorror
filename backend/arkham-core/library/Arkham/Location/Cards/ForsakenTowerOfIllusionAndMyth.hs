module Arkham.Location.Cards.ForsakenTowerOfIllusionAndMyth
  ( forsakenTowerOfIllusionAndMyth
  , ForsakenTowerOfIllusionAndMyth(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ForsakenTowerOfIllusionAndMyth = ForsakenTowerOfIllusionAndMyth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forsakenTowerOfIllusionAndMyth :: LocationCard ForsakenTowerOfIllusionAndMyth
forsakenTowerOfIllusionAndMyth = location ForsakenTowerOfIllusionAndMyth Cards.forsakenTowerOfIllusionAndMyth 5 (PerPlayer 1)

instance HasAbilities ForsakenTowerOfIllusionAndMyth where
  getAbilities (ForsakenTowerOfIllusionAndMyth attrs) =
    extendRevealed attrs []

instance RunMessage ForsakenTowerOfIllusionAndMyth where
  runMessage msg (ForsakenTowerOfIllusionAndMyth attrs) = runQueueT $ case msg of
    _ -> ForsakenTowerOfIllusionAndMyth <$> lift (runMessage msg attrs)

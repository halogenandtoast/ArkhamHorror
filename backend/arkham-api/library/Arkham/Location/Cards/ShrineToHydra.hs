module Arkham.Location.Cards.ShrineToHydra
  ( shrineToHydra
  , ShrineToHydra(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ShrineToHydra = ShrineToHydra LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shrineToHydra :: LocationCard ShrineToHydra
shrineToHydra = location ShrineToHydra Cards.shrineToHydra 0 (Static 0)

instance HasAbilities ShrineToHydra where
  getAbilities (ShrineToHydra attrs) =
    extendRevealed attrs []

instance RunMessage ShrineToHydra where
  runMessage msg (ShrineToHydra attrs) = runQueueT $ case msg of
    _ -> ShrineToHydra <$> liftRunMessage msg attrs

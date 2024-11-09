module Arkham.Location.Cards.LairOfHydra
  ( lairOfHydra
  , LairOfHydra(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LairOfHydra = LairOfHydra LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lairOfHydra :: LocationCard LairOfHydra
lairOfHydra = location LairOfHydra Cards.lairOfHydra 0 (Static 0)

instance HasAbilities LairOfHydra where
  getAbilities (LairOfHydra attrs) =
    extendRevealed attrs []

instance RunMessage LairOfHydra where
  runMessage msg (LairOfHydra attrs) = runQueueT $ case msg of
    _ -> LairOfHydra <$> liftRunMessage msg attrs

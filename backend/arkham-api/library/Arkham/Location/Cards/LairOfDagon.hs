module Arkham.Location.Cards.LairOfDagon
  ( lairOfDagon
  , LairOfDagon(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LairOfDagon = LairOfDagon LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lairOfDagon :: LocationCard LairOfDagon
lairOfDagon = location LairOfDagon Cards.lairOfDagon 3 (PerPlayer 3)

instance HasAbilities LairOfDagon where
  getAbilities (LairOfDagon attrs) =
    extendRevealed attrs []

instance RunMessage LairOfDagon where
  runMessage msg (LairOfDagon attrs) = runQueueT $ case msg of
    _ -> LairOfDagon <$> liftRunMessage msg attrs

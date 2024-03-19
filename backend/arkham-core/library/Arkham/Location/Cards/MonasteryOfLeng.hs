module Arkham.Location.Cards.MonasteryOfLeng
  ( monasteryOfLeng
  , MonasteryOfLeng(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MonasteryOfLeng = MonasteryOfLeng LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

monasteryOfLeng :: LocationCard MonasteryOfLeng
monasteryOfLeng = location MonasteryOfLeng Cards.monasteryOfLeng 3 (PerPlayer 2)

instance HasAbilities MonasteryOfLeng where
  getAbilities (MonasteryOfLeng attrs) =
    extendRevealed attrs []

instance RunMessage MonasteryOfLeng where
  runMessage msg (MonasteryOfLeng attrs) = runQueueT $ case msg of
    _ -> MonasteryOfLeng <$> lift (runMessage msg attrs)

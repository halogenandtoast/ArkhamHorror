module Arkham.Location.Cards.ShorewardSlums
  ( shorewardSlums
  , ShorewardSlums(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ShorewardSlums = ShorewardSlums LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shorewardSlums :: LocationCard ShorewardSlums
shorewardSlums = location ShorewardSlums Cards.shorewardSlums 5 (PerPlayer 1)

instance HasAbilities ShorewardSlums where
  getAbilities (ShorewardSlums attrs) =
    extendRevealed attrs []

instance RunMessage ShorewardSlums where
  runMessage msg (ShorewardSlums attrs) = runQueueT $ case msg of
    _ -> ShorewardSlums <$> liftRunMessage msg attrs

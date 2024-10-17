module Arkham.Location.Cards.DeepOneNursery
  ( deepOneNursery
  , DeepOneNursery(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype DeepOneNursery = DeepOneNursery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepOneNursery :: LocationCard DeepOneNursery
deepOneNursery = location DeepOneNursery Cards.deepOneNursery 0 (Static 0)

instance HasAbilities DeepOneNursery where
  getAbilities (DeepOneNursery attrs) =
    extendRevealed attrs []

instance RunMessage DeepOneNursery where
  runMessage msg (DeepOneNursery attrs) = runQueueT $ case msg of
    _ -> DeepOneNursery <$> liftRunMessage msg attrs

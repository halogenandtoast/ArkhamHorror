module Arkham.Location.Cards.DeepOneGrotto_175a
  ( deepOneGrotto_175a
  , DeepOneGrotto_175a(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype DeepOneGrotto_175a = DeepOneGrotto_175a LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepOneGrotto_175a :: LocationCard DeepOneGrotto_175a
deepOneGrotto_175a = location DeepOneGrotto_175a Cards.deepOneGrotto_175a 4 (PerPlayer 1)

instance HasAbilities DeepOneGrotto_175a where
  getAbilities (DeepOneGrotto_175a attrs) =
    extendRevealed attrs []

instance RunMessage DeepOneGrotto_175a where
  runMessage msg (DeepOneGrotto_175a attrs) = runQueueT $ case msg of
    _ -> DeepOneGrotto_175a <$> liftRunMessage msg attrs

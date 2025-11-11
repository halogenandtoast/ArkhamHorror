module Arkham.Treachery.Cards.DimensionalHypothesisC (dimensionalHypothesisC) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DimensionalHypothesisC = DimensionalHypothesisC TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalHypothesisC :: TreacheryCard DimensionalHypothesisC
dimensionalHypothesisC = treachery DimensionalHypothesisC Cards.dimensionalHypothesisC

instance RunMessage DimensionalHypothesisC where
  runMessage msg t@(DimensionalHypothesisC attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> DimensionalHypothesisC <$> liftRunMessage msg attrs

module Arkham.Treachery.Cards.DimensionalHypothesisA (dimensionalHypothesisA) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DimensionalHypothesisA = DimensionalHypothesisA TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalHypothesisA :: TreacheryCard DimensionalHypothesisA
dimensionalHypothesisA = treachery DimensionalHypothesisA Cards.dimensionalHypothesisA

instance RunMessage DimensionalHypothesisA where
  runMessage msg t@(DimensionalHypothesisA attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> DimensionalHypothesisA <$> liftRunMessage msg attrs

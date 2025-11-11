module Arkham.Treachery.Cards.DimensionalHypothesisB (dimensionalHypothesisB) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DimensionalHypothesisB = DimensionalHypothesisB TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalHypothesisB :: TreacheryCard DimensionalHypothesisB
dimensionalHypothesisB = treachery DimensionalHypothesisB Cards.dimensionalHypothesisB

instance RunMessage DimensionalHypothesisB where
  runMessage msg t@(DimensionalHypothesisB attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> DimensionalHypothesisB <$> liftRunMessage msg attrs

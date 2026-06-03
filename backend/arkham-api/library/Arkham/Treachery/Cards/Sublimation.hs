module Arkham.Treachery.Cards.Sublimation (sublimation) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Sublimation = Sublimation TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sublimation :: TreacheryCard Sublimation
sublimation = treachery Sublimation Cards.sublimation

instance RunMessage Sublimation where
  runMessage msg (Sublimation attrs) =
    runQueueT $ Sublimation <$> liftRunMessage msg attrs

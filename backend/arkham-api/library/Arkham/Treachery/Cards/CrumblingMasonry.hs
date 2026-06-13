module Arkham.Treachery.Cards.CrumblingMasonry (crumblingMasonry) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CrumblingMasonry = CrumblingMasonry TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crumblingMasonry :: TreacheryCard CrumblingMasonry
crumblingMasonry = treachery CrumblingMasonry Cards.crumblingMasonry

-- TODO: abilities
instance RunMessage CrumblingMasonry where
  runMessage msg (CrumblingMasonry attrs) = runQueueT $ CrumblingMasonry <$> liftRunMessage msg attrs

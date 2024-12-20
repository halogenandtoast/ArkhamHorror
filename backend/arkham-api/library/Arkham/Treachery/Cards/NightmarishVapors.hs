module Arkham.Treachery.Cards.NightmarishVapors (nightmarishVapors) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype NightmarishVapors = NightmarishVapors TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nightmarishVapors :: TreacheryCard NightmarishVapors
nightmarishVapors = treachery NightmarishVapors Cards.nightmarishVapors

instance RunMessage NightmarishVapors where
  runMessage msg t@(NightmarishVapors attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> NightmarishVapors <$> liftRunMessage msg attrs

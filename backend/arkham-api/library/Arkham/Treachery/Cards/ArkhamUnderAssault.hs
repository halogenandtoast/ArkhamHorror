module Arkham.Treachery.Cards.ArkhamUnderAssault (arkhamUnderAssault) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ArkhamUnderAssault = ArkhamUnderAssault TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamUnderAssault :: TreacheryCard ArkhamUnderAssault
arkhamUnderAssault = treachery ArkhamUnderAssault Cards.arkhamUnderAssault

-- TODO: abilities
instance RunMessage ArkhamUnderAssault where
  runMessage msg (ArkhamUnderAssault attrs) = runQueueT $ ArkhamUnderAssault <$> liftRunMessage msg attrs

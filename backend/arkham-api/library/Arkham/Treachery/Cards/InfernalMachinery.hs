module Arkham.Treachery.Cards.InfernalMachinery (infernalMachinery) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype InfernalMachinery = InfernalMachinery TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infernalMachinery :: TreacheryCard InfernalMachinery
infernalMachinery = treachery InfernalMachinery Cards.infernalMachinery

-- TODO: abilities
instance RunMessage InfernalMachinery where
  runMessage msg (InfernalMachinery attrs) = runQueueT $ InfernalMachinery <$> liftRunMessage msg attrs

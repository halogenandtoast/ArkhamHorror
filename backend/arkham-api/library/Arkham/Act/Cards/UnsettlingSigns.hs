module Arkham.Act.Cards.UnsettlingSigns (unsettlingSigns) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype UnsettlingSigns = UnsettlingSigns ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unsettlingSigns :: ActCard UnsettlingSigns
unsettlingSigns = act (1, A) UnsettlingSigns Cards.unsettlingSigns Nothing

-- TODO: abilities
instance HasAbilities UnsettlingSigns where
  getAbilities _ = []

instance RunMessage UnsettlingSigns where
  runMessage msg (UnsettlingSigns attrs) = runQueueT $ UnsettlingSigns <$> liftRunMessage msg attrs

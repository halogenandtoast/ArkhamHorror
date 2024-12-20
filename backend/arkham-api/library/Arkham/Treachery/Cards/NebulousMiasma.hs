module Arkham.Treachery.Cards.NebulousMiasma (nebulousMiasma) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype NebulousMiasma = NebulousMiasma TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nebulousMiasma :: TreacheryCard NebulousMiasma
nebulousMiasma = treachery NebulousMiasma Cards.nebulousMiasma

instance RunMessage NebulousMiasma where
  runMessage msg t@(NebulousMiasma attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> NebulousMiasma <$> liftRunMessage msg attrs

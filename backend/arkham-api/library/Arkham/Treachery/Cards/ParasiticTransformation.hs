module Arkham.Treachery.Cards.ParasiticTransformation (parasiticTransformation) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ParasiticTransformation = ParasiticTransformation TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parasiticTransformation :: TreacheryCard ParasiticTransformation
parasiticTransformation = treachery ParasiticTransformation Cards.parasiticTransformation

-- TODO: abilities
instance RunMessage ParasiticTransformation where
  runMessage msg (ParasiticTransformation attrs) = runQueueT $ ParasiticTransformation <$> liftRunMessage msg attrs

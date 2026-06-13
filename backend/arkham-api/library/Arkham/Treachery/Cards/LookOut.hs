module Arkham.Treachery.Cards.LookOut (lookOut) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LookOut = LookOut TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lookOut :: TreacheryCard LookOut
lookOut = treachery LookOut Cards.lookOut

-- TODO: abilities
instance RunMessage LookOut where
  runMessage msg (LookOut attrs) = runQueueT $ LookOut <$> liftRunMessage msg attrs

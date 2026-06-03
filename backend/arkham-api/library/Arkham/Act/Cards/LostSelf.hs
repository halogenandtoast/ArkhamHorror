module Arkham.Act.Cards.LostSelf (lostSelf) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype LostSelf = LostSelf ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostSelf :: ActCard LostSelf
lostSelf = act (2, A) LostSelf Cards.lostSelf Nothing

instance RunMessage LostSelf where
  runMessage msg (LostSelf attrs) =
    runQueueT $ LostSelf <$> liftRunMessage msg attrs

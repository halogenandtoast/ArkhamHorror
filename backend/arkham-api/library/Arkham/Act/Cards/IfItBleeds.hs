module Arkham.Act.Cards.IfItBleeds (ifItBleeds) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype IfItBleeds = IfItBleeds ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ifItBleeds :: ActCard IfItBleeds
ifItBleeds = act (2, A) IfItBleeds Cards.ifItBleeds Nothing

instance RunMessage IfItBleeds where
  runMessage msg a@(IfItBleeds attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> IfItBleeds <$> liftRunMessage msg attrs

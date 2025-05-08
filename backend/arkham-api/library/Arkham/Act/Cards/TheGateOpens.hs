module Arkham.Act.Cards.TheGateOpens (theGateOpens) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheGateOpens = TheGateOpens ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theGateOpens :: ActCard TheGateOpens
theGateOpens =
  act (3, A) TheGateOpens Cards.theGateOpens (Just $ GroupClueCost (PerPlayer 2) "Sentinel Peak")

instance RunMessage TheGateOpens where
  runMessage msg a@(TheGateOpens attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> TheGateOpens <$> liftRunMessage msg attrs

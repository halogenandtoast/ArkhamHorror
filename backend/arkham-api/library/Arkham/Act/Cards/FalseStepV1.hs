module Arkham.Act.Cards.FalseStepV1 (falseStepV1) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype FalseStepV1 = FalseStepV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

falseStepV1 :: ActCard FalseStepV1
falseStepV1 = act (1, A) FalseStepV1 Cards.falseStepV1 Nothing

instance RunMessage FalseStepV1 where
  runMessage msg a@(FalseStepV1 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> FalseStepV1 <$> liftRunMessage msg attrs

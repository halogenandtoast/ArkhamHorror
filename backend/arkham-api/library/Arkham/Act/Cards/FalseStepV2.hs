module Arkham.Act.Cards.FalseStepV2 (falseStepV2) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype FalseStepV2 = FalseStepV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

falseStepV2 :: ActCard FalseStepV2
falseStepV2 = act (1, A) FalseStepV2 Cards.falseStepV2 Nothing

instance RunMessage FalseStepV2 where
  runMessage msg a@(FalseStepV2 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> FalseStepV2 <$> liftRunMessage msg attrs

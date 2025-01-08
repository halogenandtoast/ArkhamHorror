module Arkham.Act.Cards.ShadowOfThePastV1 (shadowOfThePastV1) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype ShadowOfThePastV1 = ShadowOfThePastV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

shadowOfThePastV1 :: ActCard ShadowOfThePastV1
shadowOfThePastV1 = act (1, A) ShadowOfThePastV1 Cards.shadowOfThePastV1 Nothing

instance RunMessage ShadowOfThePastV1 where
  runMessage msg a@(ShadowOfThePastV1 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> ShadowOfThePastV1 <$> liftRunMessage msg attrs

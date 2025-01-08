module Arkham.Act.Cards.ShadowOfThePastV2 (shadowOfThePastV2) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype ShadowOfThePastV2 = ShadowOfThePastV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

shadowOfThePastV2 :: ActCard ShadowOfThePastV2
shadowOfThePastV2 = act (1, A) ShadowOfThePastV2 Cards.shadowOfThePastV2 Nothing

instance RunMessage ShadowOfThePastV2 where
  runMessage msg a@(ShadowOfThePastV2 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> ShadowOfThePastV2 <$> liftRunMessage msg attrs

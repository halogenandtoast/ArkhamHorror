module Arkham.Act.Cards.ShadowOfThePastV3 (shadowOfThePastV3) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype ShadowOfThePastV3 = ShadowOfThePastV3 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

shadowOfThePastV3 :: ActCard ShadowOfThePastV3
shadowOfThePastV3 = act (1, A) ShadowOfThePastV3 Cards.shadowOfThePastV3 Nothing

instance RunMessage ShadowOfThePastV3 where
  runMessage msg a@(ShadowOfThePastV3 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> ShadowOfThePastV3 <$> liftRunMessage msg attrs

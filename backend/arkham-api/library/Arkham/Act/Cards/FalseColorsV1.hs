module Arkham.Act.Cards.FalseColorsV1 (falseColorsV1) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype FalseColorsV1 = FalseColorsV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

falseColorsV1 :: ActCard FalseColorsV1
falseColorsV1 = act (2, A) FalseColorsV1 Cards.falseColorsV1 Nothing

instance RunMessage FalseColorsV1 where
  runMessage msg a@(FalseColorsV1 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> FalseColorsV1 <$> liftRunMessage msg attrs

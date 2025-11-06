module Arkham.Act.Cards.FalseColorsV2 (falseColorsV2) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype FalseColorsV2 = FalseColorsV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

falseColorsV2 :: ActCard FalseColorsV2
falseColorsV2 = act (2, A) FalseColorsV2 Cards.falseColorsV2 Nothing

instance RunMessage FalseColorsV2 where
  runMessage msg a@(FalseColorsV2 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> FalseColorsV2 <$> liftRunMessage msg attrs

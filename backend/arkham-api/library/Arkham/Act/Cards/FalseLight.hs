module Arkham.Act.Cards.FalseLight (falseLight) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype FalseLight = FalseLight ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

falseLight :: ActCard FalseLight
falseLight = act (3, A) FalseLight Cards.falseLight Nothing

instance RunMessage FalseLight where
  runMessage msg a@(FalseLight attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> FalseLight <$> liftRunMessage msg attrs

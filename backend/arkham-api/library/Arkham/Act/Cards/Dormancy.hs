module Arkham.Act.Cards.Dormancy (dormancy) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype Dormancy = Dormancy ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dormancy :: ActCard Dormancy
dormancy = act (1, A) Dormancy Cards.dormancy Nothing

instance RunMessage Dormancy where
  runMessage msg a@(Dormancy attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> Dormancy <$> liftRunMessage msg attrs

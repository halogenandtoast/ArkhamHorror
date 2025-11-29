module Arkham.Act.Cards.RabbitsWhoRunV1 (rabbitsWhoRunV1) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype RabbitsWhoRunV1 = RabbitsWhoRunV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

rabbitsWhoRunV1 :: ActCard RabbitsWhoRunV1
rabbitsWhoRunV1 = act (1, A) RabbitsWhoRunV1 Cards.rabbitsWhoRunV1 Nothing

instance RunMessage RabbitsWhoRunV1 where
  runMessage msg a@(RabbitsWhoRunV1 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> RabbitsWhoRunV1 <$> liftRunMessage msg attrs

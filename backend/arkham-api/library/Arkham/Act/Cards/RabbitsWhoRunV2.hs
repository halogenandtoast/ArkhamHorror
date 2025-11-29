module Arkham.Act.Cards.RabbitsWhoRunV2 (rabbitsWhoRunV2) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype RabbitsWhoRunV2 = RabbitsWhoRunV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

rabbitsWhoRunV2 :: ActCard RabbitsWhoRunV2
rabbitsWhoRunV2 = act (1, A) RabbitsWhoRunV2 Cards.rabbitsWhoRunV2 Nothing

instance RunMessage RabbitsWhoRunV2 where
  runMessage msg a@(RabbitsWhoRunV2 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> RabbitsWhoRunV2 <$> liftRunMessage msg attrs

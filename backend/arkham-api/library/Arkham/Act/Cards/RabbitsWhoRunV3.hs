module Arkham.Act.Cards.RabbitsWhoRunV3 (rabbitsWhoRunV3) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype RabbitsWhoRunV3 = RabbitsWhoRunV3 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

rabbitsWhoRunV3 :: ActCard RabbitsWhoRunV3
rabbitsWhoRunV3 = act (1, A) RabbitsWhoRunV3 Cards.rabbitsWhoRunV3 Nothing

instance RunMessage RabbitsWhoRunV3 where
  runMessage msg a@(RabbitsWhoRunV3 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> RabbitsWhoRunV3 <$> liftRunMessage msg attrs

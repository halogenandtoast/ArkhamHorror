module Arkham.Act.Cards.TheStrangerHereIsMyReply (theStrangerHereIsMyReply) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheStrangerHereIsMyReply = TheStrangerHereIsMyReply ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theStrangerHereIsMyReply :: ActCard TheStrangerHereIsMyReply
theStrangerHereIsMyReply = act (1, A) TheStrangerHereIsMyReply Cards.theStrangerHereIsMyReply Nothing

instance RunMessage TheStrangerHereIsMyReply where
  runMessage msg a@(TheStrangerHereIsMyReply attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheStrangerHereIsMyReply <$> liftRunMessage msg attrs

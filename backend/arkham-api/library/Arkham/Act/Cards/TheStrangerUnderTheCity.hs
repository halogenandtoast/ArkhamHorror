module Arkham.Act.Cards.TheStrangerUnderTheCity (theStrangerUnderTheCity) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheStrangerUnderTheCity = TheStrangerUnderTheCity ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theStrangerUnderTheCity :: ActCard TheStrangerUnderTheCity
theStrangerUnderTheCity = act (1, A) TheStrangerUnderTheCity Cards.theStrangerUnderTheCity Nothing

instance RunMessage TheStrangerUnderTheCity where
  runMessage msg a@(TheStrangerUnderTheCity attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheStrangerUnderTheCity <$> liftRunMessage msg attrs

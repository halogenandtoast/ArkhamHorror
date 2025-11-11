module Arkham.Act.Cards.TheExit (theExit) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheExit = TheExit ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theExit :: ActCard TheExit
theExit = act (3, A) TheExit Cards.theExit Nothing

instance RunMessage TheExit where
  runMessage msg a@(TheExit attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheExit <$> liftRunMessage msg attrs

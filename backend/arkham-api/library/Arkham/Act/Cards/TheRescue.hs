module Arkham.Act.Cards.TheRescue
  ( TheRescue(..)
  , theRescue
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheRescue = TheRescue ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theRescue :: ActCard TheRescue
theRescue = act (1, A) TheRescue Cards.theRescue Nothing

instance RunMessage TheRescue where
  runMessage msg a@(TheRescue attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheRescue <$> liftRunMessage msg attrs

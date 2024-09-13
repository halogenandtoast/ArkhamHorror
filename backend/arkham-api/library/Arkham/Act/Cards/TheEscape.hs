module Arkham.Act.Cards.TheEscape
  ( TheEscape(..)
  , theEscape
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheEscape = TheEscape ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theEscape :: ActCard TheEscape
theEscape = act (2, A) TheEscape Cards.theEscape Nothing

instance RunMessage TheEscape where
  runMessage msg a@(TheEscape attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheEscape <$> liftRunMessage msg attrs

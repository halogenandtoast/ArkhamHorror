module Arkham.Act.Cards.TheSearchForAgentHarper
  ( TheSearchForAgentHarper(..)
  , theSearchForAgentHarper
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheSearchForAgentHarper = TheSearchForAgentHarper ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theSearchForAgentHarper :: ActCard TheSearchForAgentHarper
theSearchForAgentHarper = act (1, A) TheSearchForAgentHarper Cards.theSearchForAgentHarper Nothing

instance RunMessage TheSearchForAgentHarper where
  runMessage msg a@(TheSearchForAgentHarper attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheSearchForAgentHarper <$> liftRunMessage msg attrs

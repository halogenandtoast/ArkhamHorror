module Arkham.Helpers.ChaosBag.Lifted where

import Arkham.ChaosBag.RevealStrategy
import Arkham.Classes.HasQueue (push)
import Arkham.Id
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Queue
import Arkham.Prelude
import Arkham.RequestedChaosTokenStrategy
import Arkham.Source

revealChaosTokens :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> Int -> m ()
revealChaosTokens source iid n = do
  push $ Msg.RequestChaosTokens (toSource source) (Just iid) (Reveal n) SetAside

resetChaosTokens :: (ReverseQueue m, Sourceable source) => source -> m ()
resetChaosTokens source = push $ Msg.ResetChaosTokens (toSource source)

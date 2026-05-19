module Arkham.Helpers.ChaosBag.Lifted where

import Arkham.Classes.HasQueue (push)
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Queue
import Arkham.Prelude
import Arkham.Source

resetChaosTokens :: (ReverseQueue m, Sourceable source) => source -> m ()
resetChaosTokens source = push $ Msg.ResetChaosTokens (toSource source)

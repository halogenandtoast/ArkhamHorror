{- | Named scenario-owned bags. Queued writes, like other scenario state, become
visible when their messages resolve; thread the returned bag through any
multi-draw operation rather than re-reading it between queued writes.
-}
module Arkham.Helpers.CustomChaosBag where

import Arkham.ChaosToken.Types (ChaosTokenFace)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Helpers.Scenario
import Arkham.Message (Message (RemoveCustomChaosBag, SetCustomChaosBag))
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Prelude
import Arkham.Scenario.Types (Field (ScenarioCustomChaosBags))
import Arkham.TokenBag
import Data.Map.Strict qualified as Map

getCustomChaosBag :: HasGame m => Text -> m CustomChaosBag
getCustomChaosBag name =
  scenarioFieldMap ScenarioCustomChaosBags
    $ fromJustNote ("Missing custom chaos bag: " <> unpack name)
    . Map.lookup name

setCustomChaosBag :: ReverseQueue m => Text -> CustomChaosBag -> m ()
setCustomChaosBag name = push . SetCustomChaosBag name

initCustomChaosBag :: ReverseQueue m => Text -> [ChaosTokenFace] -> m ()
initCustomChaosBag name faces = initTokenBag faces >>= setCustomChaosBag name

removeCustomChaosBag :: ReverseQueue m => Text -> m ()
removeCustomChaosBag = push . RemoveCustomChaosBag

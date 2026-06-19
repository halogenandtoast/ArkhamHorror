module Arkham.Scenarios.BadBlood.Helpers where

import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Message (Message (ScenarioSpecific))
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Scenario.Types (Field (..))
import Arkham.Scenarios.BadBlood.Meta
import Arkham.Token (Token (Memory))
import Arkham.Tracing

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ standaloneI18n "badBlood" a

getBadBloodMeta :: (HasGame m, Tracing m) => m Meta
getBadBloodMeta = scenarioFieldMap ScenarioMeta (toResultDefault emptyMeta)

agnesBaker :: InvestigatorMatcher
agnesBaker = InvestigatorWithTitle "Agnes Baker"

memoryLocation :: LocationMatcher
memoryLocation = LocationWithToken Memory

agnesCollectsMemoryAt :: ReverseQueue m => LocationId -> m ()
agnesCollectsMemoryAt lid = push $ ScenarioSpecific "agnesCollectsMemory" (toJSON lid)

elspethCollectsMemoryAt :: ReverseQueue m => LocationId -> m ()
elspethCollectsMemoryAt lid = push $ ScenarioSpecific "elspethCollectsMemory" (toJSON lid)

agnesStealsMemory :: ReverseQueue m => m ()
agnesStealsMemory = push $ ScenarioSpecific "agnesStealsMemory" Null

checkMemoryTokens :: ReverseQueue m => m ()
checkMemoryTokens = push $ ScenarioSpecific "checkMemoryTokens" Null

module Arkham.Scenarios.DarkSideOfTheMoon.Helpers where

import Arkham.Classes.HasGame
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Token

getAlarmLevel :: HasGame m => InvestigatorId -> m Int
getAlarmLevel = fieldMap InvestigatorTokens (countTokens AlarmLevel)
{-# INLINE getAlarmLevel #-}

raiseAlarmLevel :: (Sourceable source, ReverseQueue m) => source -> InvestigatorId -> m ()
raiseAlarmLevel source = raiseAlarmLevelBy 1 source
{-# INLINE raiseAlarmLevel #-}

raiseAlarmLevelBy :: (Sourceable source, ReverseQueue m) => Int -> source -> InvestigatorId -> m ()
raiseAlarmLevelBy n (toSource -> source) iid = placeTokens source iid AlarmLevel n
{-# INLINE raiseAlarmLevelBy #-}

reduceAlarmLevel :: (Sourceable source, ReverseQueue m) => source -> InvestigatorId -> m ()
reduceAlarmLevel source = reduceAlarmLevelBy 1 source
{-# INLINE reduceAlarmLevel #-}

reduceAlarmLevelBy :: (Sourceable source, ReverseQueue m) => Int -> source -> InvestigatorId -> m ()
reduceAlarmLevelBy n (toSource -> source) iid = removeTokens source iid AlarmLevel n
{-# INLINE reduceAlarmLevelBy #-}

module Arkham.Scenarios.DarkSideOfTheMoon.Helpers where

import Arkham.Classes.HasGame
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Token
import Arkham.Window

getAlarmLevel :: HasGame m => InvestigatorId -> m Int
getAlarmLevel = fieldMap InvestigatorTokens (countTokens AlarmLevel)
{-# INLINE getAlarmLevel #-}

raiseAlarmLevel :: (Sourceable source, ReverseQueue m) => source -> [InvestigatorId] -> m ()
raiseAlarmLevel source iids = do
  for_ iids $ \iid -> placeTokens source iid AlarmLevel 1
  checkWindows $ mkAfter <$> map IncreasedAlarmLevel iids
{-# INLINE raiseAlarmLevel #-}

reduceAlarmLevel :: (Sourceable source, ReverseQueue m) => source -> InvestigatorId -> m ()
reduceAlarmLevel source = reduceAlarmLevelBy 1 source
{-# INLINE reduceAlarmLevel #-}

reduceAlarmLevelBy :: (Sourceable source, ReverseQueue m) => Int -> source -> InvestigatorId -> m ()
reduceAlarmLevelBy n (toSource -> source) iid = removeTokens source iid AlarmLevel n
{-# INLINE reduceAlarmLevelBy #-}

getMaxAlarmLevel :: HasGame m => m Int
getMaxAlarmLevel = do
  investigators <- getInvestigators
  alarmLevels <- traverse (fieldMap InvestigatorTokens (countTokens AlarmLevel)) investigators
  pure $ getMax0 $ foldMap Max0 alarmLevels

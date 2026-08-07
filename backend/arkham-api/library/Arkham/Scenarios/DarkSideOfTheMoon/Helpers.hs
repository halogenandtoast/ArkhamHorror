module Arkham.Scenarios.DarkSideOfTheMoon.Helpers where

import Arkham.Campaigns.TheDreamEaters.Helpers
import Arkham.Classes.HasGame
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Query (getInvestigators)
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Token
import Arkham.Tracing
import Arkham.Window

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "darkSideOfTheMoon" a

getAlarmLevel :: (HasGame m, Tracing m) => InvestigatorId -> m Int
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

{- | Dark Side of the Moon puts no floor on an alarm level: zero is a reachable
(and achievement-worthy) state. Fortune and Folly's alarm level is the one that
"cannot be reduced below 1 or raised above 10", so that scenario keeps its own
clamped copy of this helper rather than sharing this one.
-}
reduceAlarmLevelBy :: (Sourceable source, ReverseQueue m) => Int -> source -> InvestigatorId -> m ()
reduceAlarmLevelBy n (toSource -> source) iid = do
  current <- getAlarmLevel iid
  let n' = min n current
  when (n' > 0) $ removeTokens source iid AlarmLevel n'
{-# INLINE reduceAlarmLevelBy #-}

getMaxAlarmLevel :: (HasGame m, Tracing m) => m Int
getMaxAlarmLevel = do
  investigators <- getInvestigators
  alarmLevels <- traverse (fieldMap InvestigatorTokens (countTokens AlarmLevel)) investigators
  pure $ getMax0 $ foldMap Max0 alarmLevels

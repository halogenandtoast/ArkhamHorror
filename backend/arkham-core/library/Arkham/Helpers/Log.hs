module Arkham.Helpers.Log where

import Arkham.Prelude

import Arkham.Campaign.Types (Field (..))
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Scenario
import Arkham.Message
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.ScenarioLogKey
import Arkham.Store

getCampaignLog :: (HasGame m, Store m Card) => m CampaignLog
getCampaignLog =
  withStandalone
    (field CampaignCampaignLog)
    (field ScenarioStandaloneCampaignLog)

getHasRecord :: (HasGame m, Store m Card) => CampaignLogKey -> m Bool
getHasRecord k = do
  campaignLog <- getCampaignLog
  pure $
    or
      [ k `member` campaignLogRecorded campaignLog
      , k `member` campaignLogRecordedCounts campaignLog
      ]

whenHasRecord :: (HasGame m, Store m Card) => CampaignLogKey -> m () -> m ()
whenHasRecord k = whenM (getHasRecord k)

getRecordCount :: (HasGame m, Store m Card) => CampaignLogKey -> m Int
getRecordCount k =
  findWithDefault 0 k . campaignLogRecordedCounts <$> getCampaignLog

getRecordSet :: (HasGame m, Store m Card) => CampaignLogKey -> m [SomeRecorded]
getRecordSet k =
  findWithDefault [] k . campaignLogRecordedSets <$> getCampaignLog

getRecordedCardCodes :: (HasGame m, Store m Card) => CampaignLogKey -> m [CardCode]
getRecordedCardCodes k = mapMaybe onlyRecorded <$> getRecordSet k
 where
  onlyRecorded :: SomeRecorded -> Maybe CardCode
  onlyRecorded = \case
    SomeRecorded RecordableCardCode (Recorded cCode) -> Just cCode
    _ -> Nothing

remembered :: (HasGame m, Store m Card) => ScenarioLogKey -> m Bool
remembered k = member k <$> scenarioField ScenarioRemembered

scenarioCount :: (HasGame m, Store m Card) => ScenarioCountKey -> m Int
scenarioCount k = fromMaybe 0 . lookup k <$> scenarioField ScenarioCounts

recordSetInsert
  :: (Recordable a, MonoFoldable t, Element t ~ a)
  => CampaignLogKey
  -> t
  -> Message
recordSetInsert k xs = RecordSetInsert k $ map recorded $ toList xs

crossOutRecordSetEntries :: (Recordable a) => CampaignLogKey -> [a] -> Message
crossOutRecordSetEntries k xs = CrossOutRecordSetEntries k $ map recorded xs

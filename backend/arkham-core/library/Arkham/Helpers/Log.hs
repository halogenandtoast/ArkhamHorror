module Arkham.Helpers.Log where

import Arkham.Prelude

import Arkham.Campaign.Types (Field (..))
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Card.CardCode
import Arkham.Classes.HasGame
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Message
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.ScenarioLogKey

getCampaignLog :: HasGame m => m CampaignLog
getCampaignLog =
  withStandalone
    (field CampaignCampaignLog)
    (field ScenarioStandaloneCampaignLog)

getInvestigatorHasRecord :: HasGame m => InvestigatorId -> CampaignLogKey -> m Bool
getInvestigatorHasRecord iid k = do
  ilog <- field InvestigatorLog iid
  pure
    $ or
      [ k `member` campaignLogRecorded ilog
      , k `member` campaignLogRecordedCounts ilog
      ]

getHasRecord :: HasGame m => CampaignLogKey -> m Bool
getHasRecord k = do
  campaignLog <- getCampaignLog
  pure
    $ or
      [ k `member` campaignLogRecorded campaignLog
      , k `member` campaignLogRecordedCounts campaignLog
      ]

whenHasRecord :: HasGame m => CampaignLogKey -> m () -> m ()
whenHasRecord k = whenM (getHasRecord k)

getRecordCount :: HasGame m => CampaignLogKey -> m Int
getRecordCount k =
  findWithDefault 0 k . campaignLogRecordedCounts <$> getCampaignLog

getRecordSet :: HasGame m => CampaignLogKey -> m [SomeRecorded]
getRecordSet k =
  findWithDefault [] k . campaignLogRecordedSets <$> getCampaignLog

inRecordSet :: (Recordable a, HasGame m) => a -> CampaignLogKey -> m Bool
inRecordSet v k = do
  recordSet <- getRecordSet k
  pure $ recorded v `elem` recordSet

getRecordedCardCodes :: HasGame m => CampaignLogKey -> m [CardCode]
getRecordedCardCodes k = mapMaybe onlyRecorded <$> getRecordSet k
 where
  onlyRecorded :: SomeRecorded -> Maybe CardCode
  onlyRecorded = \case
    SomeRecorded RecordableCardCode (Recorded cCode) -> Just cCode
    _ -> Nothing

getCrossedOutCardCodes :: HasGame m => CampaignLogKey -> m [CardCode]
getCrossedOutCardCodes k = mapMaybe onlyCrossedOut <$> getRecordSet k
 where
  onlyCrossedOut :: SomeRecorded -> Maybe CardCode
  onlyCrossedOut = \case
    SomeRecorded RecordableCardCode (CrossedOut cCode) -> Just cCode
    _ -> Nothing

remembered :: HasGame m => ScenarioLogKey -> m Bool
remembered k = member k <$> scenarioField ScenarioRemembered

scenarioCount :: HasGame m => ScenarioCountKey -> m Int
scenarioCount k = fromMaybe 0 . lookup k <$> scenarioField ScenarioCounts

recordSetInsert
  :: (Recordable a, MonoFoldable t, Element t ~ a)
  => CampaignLogKey
  -> t
  -> Message
recordSetInsert k xs = RecordSetInsert k $ map recorded $ toList xs

crossOutRecordSetEntries :: Recordable a => CampaignLogKey -> [a] -> Message
crossOutRecordSetEntries k xs = CrossOutRecordSetEntries k $ map recorded xs

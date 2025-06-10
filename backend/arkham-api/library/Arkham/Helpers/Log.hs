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
import Data.Typeable

getCampaignLog :: HasGame m => m CampaignLog
getCampaignLog =
  withStandalone
    (field CampaignCampaignLog)
    (field ScenarioStandaloneCampaignLog)

getInvestigatorHasRecord :: (HasGame m, IsCampaignLogKey k) => InvestigatorId -> k -> m Bool
getInvestigatorHasRecord iid k = fieldMap InvestigatorLog (hasRecord k) iid

getHasRecord :: (HasGame m, IsCampaignLogKey k) => k -> m Bool
getHasRecord k = hasRecord k <$> getCampaignLog

hasRecord :: (IsCampaignLogKey k) => k -> CampaignLog -> Bool
hasRecord (toCampaignLogKey -> k) campaignLog =
  or
    [ k `member` campaignLogRecorded campaignLog
    , k `member` campaignLogRecordedCounts campaignLog
    ]

whenHasRecord :: (HasGame m, IsCampaignLogKey k) => k -> m () -> m ()
whenHasRecord k = whenM (getHasRecord k)

unlessHasRecord :: (HasGame m, IsCampaignLogKey k) => k -> m () -> m ()
unlessHasRecord k = unlessM (getHasRecord k)

getRecordCount :: (IsCampaignLogKey k, HasGame m) => k -> m Int
getRecordCount k =
  findWithDefault 0 (toCampaignLogKey k) . campaignLogRecordedCounts <$> getCampaignLog

getRecordSet :: (HasGame m, IsCampaignLogKey k) => k -> m [SomeRecorded]
getRecordSet k =
  findWithDefault [] (toCampaignLogKey k) . campaignLogRecordedSets <$> getCampaignLog

getSomeRecordSet :: forall a k m. (HasGame m, Recordable a, IsCampaignLogKey k) => k -> m [a]
getSomeRecordSet k = do
  srs <- findWithDefault [] (toCampaignLogKey k) . campaignLogRecordedSets <$> getCampaignLog
  pure $ flip mapMaybe srs \case
    SomeRecorded _ (Recorded r :: Recorded r) -> case eqT @a @r of
      Just Refl -> Just r
      Nothing -> Nothing
    _ -> Nothing

getSomeRecordSetJSON :: forall a k m. (HasGame m, FromJSON a, IsCampaignLogKey k) => k -> m [a]
getSomeRecordSetJSON k = do
  srs <- findWithDefault [] (toCampaignLogKey k) . campaignLogRecordedSets <$> getCampaignLog
  pure $ flip mapMaybe srs \case
    SomeRecorded RecordableGeneric (Recorded r :: Recorded r) -> maybeResult r
    _ -> Nothing

inRecordSet :: (Recordable a, HasGame m, IsCampaignLogKey k) => a -> k -> m Bool
inRecordSet v k = do
  recordSet <- getRecordSet k
  pure $ recorded v `elem` recordSet

getCircledRecord :: forall a k m. (Recordable a, HasGame m, IsCampaignLogKey k) => k -> m (Maybe a)
getCircledRecord k = do
  rs <- getRecordSet k
  pure $ case mapMaybe isCircled rs of
    (x : _) -> Just x
    _ -> Nothing
 where
  isCircled = \case
    SomeRecorded _ (Circled (Recorded a :: Recorded b)) -> case eqT @a @b of
      Just Refl -> Just a
      Nothing -> Nothing
    _ -> Nothing

getRecordedCardCodes :: (HasGame m, IsCampaignLogKey k) => k -> m [CardCode]
getRecordedCardCodes k = mapMaybe onlyRecorded <$> getRecordSet k
 where
  onlyRecorded :: SomeRecorded -> Maybe CardCode
  onlyRecorded = \case
    SomeRecorded RecordableCardCode (Recorded cCode) -> Just cCode
    _ -> Nothing

getCrossedOutCardCodes :: (HasGame m, IsCampaignLogKey k) => k -> m [CardCode]
getCrossedOutCardCodes k = mapMaybe onlyCrossedOut <$> getRecordSet k
 where
  onlyCrossedOut :: SomeRecorded -> Maybe CardCode
  onlyCrossedOut = \case
    SomeRecorded RecordableCardCode (CrossedOut cCode) -> Just cCode
    _ -> Nothing

remembered :: HasGame m => ScenarioLogKey -> m Bool
remembered k = member k <$> scenarioField ScenarioRemembered

whenRemembered :: HasGame m => ScenarioLogKey -> m () -> m ()
whenRemembered k = whenM (remembered k)

scenarioCount :: HasGame m => ScenarioCountKey -> m Int
scenarioCount k = fromMaybe 0 . lookup k <$> scenarioField ScenarioCounts

recordSetInsert
  :: (Recordable a, MonoFoldable t, Element t ~ a, IsCampaignLogKey k)
  => k
  -> t
  -> Message
recordSetInsert k xs = RecordSetInsert (toCampaignLogKey k) $ map recorded $ toList xs

recordSetReplace :: (IsCampaignLogKey k) => k -> SomeRecorded -> SomeRecorded -> Message
recordSetReplace k v v' = RecordSetReplace (toCampaignLogKey k) v v'

crossOutRecordSetEntries :: (Recordable a, IsCampaignLogKey k) => k -> [a] -> Message
crossOutRecordSetEntries k xs = CrossOutRecordSetEntries (toCampaignLogKey k) $ map recorded xs


{-# LANGUAGE TemplateHaskell #-}

module Arkham.CampaignLog where

import Arkham.Campaign.Option
import Arkham.CampaignLogKey
import Arkham.Card.CardCode
import Arkham.Json
import Arkham.Prelude
import Data.Aeson.TH
import GHC.Records

data PartnerStatus = Eliminated | Resolute | Mia | Safe | Victim | CannotTake | TheEntity
  deriving stock (Show, Generic, Eq, Data)

data CampaignLogPartner = CampaignLogPartner
  { campaignLogPartnerDamage :: Int
  , campaignLogPartnerHorror :: Int
  , campaignLogPartnerStatus :: PartnerStatus
  }
  deriving stock (Show, Generic, Eq, Data)

instance HasField "status" CampaignLogPartner PartnerStatus where
  getField = campaignLogPartnerStatus

instance HasField "damage" CampaignLogPartner Int where
  getField = campaignLogPartnerDamage

instance HasField "horror" CampaignLogPartner Int where
  getField = campaignLogPartnerHorror

data CampaignLog = CampaignLog
  { campaignLogRecorded :: Set CampaignLogKey
  , campaignLogCrossedOut :: Set CampaignLogKey
  , campaignLogRecordedCounts :: Map CampaignLogKey Int
  , campaignLogRecordedSets :: Map CampaignLogKey [SomeRecorded]
  , campaignLogOrderedKeys :: [CampaignLogKey]
  , campaignLogOptions :: Set CampaignOption
  , campaignLogPartners :: Map CardCode CampaignLogPartner
  }
  deriving stock (Show, Generic, Eq, Data)

instance HasField "recorded" CampaignLog (Set CampaignLogKey) where
  getField = campaignLogRecorded

instance HasField "recordedCounts" CampaignLog (Map CampaignLogKey Int) where
  getField = campaignLogRecordedCounts

instance Semigroup CampaignLog where
  a <> b =
    CampaignLog
      { campaignLogRecorded = campaignLogRecorded a <> campaignLogRecorded b
      , campaignLogCrossedOut = campaignLogCrossedOut a <> campaignLogCrossedOut b
      , campaignLogRecordedCounts = campaignLogRecordedCounts a <> campaignLogRecordedCounts b
      , campaignLogRecordedSets = campaignLogRecordedSets a <> campaignLogRecordedSets b
      , campaignLogOrderedKeys = campaignLogOrderedKeys a <> campaignLogOrderedKeys b
      , campaignLogOptions = campaignLogOptions a <> campaignLogOptions b
      , campaignLogPartners = campaignLogPartners a <> campaignLogPartners b
      }

instance Monoid CampaignLog where
  mempty = mkCampaignLog

emptyCampaignLog :: CampaignLog -> Bool
emptyCampaignLog CampaignLog {..} =
  null campaignLogRecorded
    && null campaignLogCrossedOut
    && null campaignLogRecordedCounts
    && null campaignLogRecordedSets
    && null campaignLogOrderedKeys
    && null campaignLogOptions
    && null campaignLogPartners

mkCampaignLog :: CampaignLog
mkCampaignLog =
  CampaignLog
    { campaignLogRecorded = mempty
    , campaignLogCrossedOut = mempty
    , campaignLogRecordedCounts = mempty
    , campaignLogRecordedSets = mempty
    , campaignLogOrderedKeys = mempty
    , campaignLogOptions = mempty
    , campaignLogPartners = mempty
    }

mconcat
  [ makeLensesWith (suffixedWithFields "campaignLog") ''CampaignLog
  , makeLensesWith (suffixedWithFields "campaignLogPartner") ''CampaignLogPartner
  , deriveJSON defaultOptions ''PartnerStatus
  , deriveJSON (aesonOptions $ Just "campaignLogPartner") ''CampaignLogPartner
  , deriveToJSON (aesonOptions $ Just "campaignLog") ''CampaignLog
  ]

instance FromJSON CampaignLog where
  parseJSON = withObject "CampaignLog" \o -> do
    campaignLogRecorded <- o .: "recorded"
    campaignLogCrossedOut <- o .: "crossedOut"
    campaignLogRecordedCounts <- o .: "recordedCounts"
    campaignLogRecordedSets <- o .: "recordedSets"
    campaignLogOrderedKeys <- o .: "orderedKeys"
    campaignLogOptions <- o .: "options"
    campaignLogPartners <- o .:? "partners" .!= mempty
    pure $ CampaignLog {..}

setCampaignLogKey :: CampaignLogKey -> CampaignLog -> CampaignLog
setCampaignLogKey key = recordedL %~ insertSet key

deleteCampaignLogKey :: CampaignLogKey -> CampaignLog -> CampaignLog
deleteCampaignLogKey key = recordedL %~ deleteSet key

setCampaignLogRecorded :: CampaignLogKey -> [SomeRecorded] -> CampaignLog -> CampaignLog
setCampaignLogRecorded key entries = recordedSetsL %~ insertMap key entries

setCampaignLogRecordedCount :: CampaignLogKey -> Int -> CampaignLog -> CampaignLog
setCampaignLogRecordedCount key n = recordedCountsL %~ insertMap key n

setCampaignLogOption :: CampaignOption -> CampaignLog -> CampaignLog
setCampaignLogOption key = optionsL %~ insertSet key

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Arkham.Types (
  Token(..),
  Scenario(..),
  Cycle(..),
  GameSettings(..),
  module Arkham.BasicTypes
) where

import           Data.Aeson       (withObject)
import           Data.Aeson.Types (ToJSONKey)
import           Data.Text
import           Import
import           Arkham.BasicTypes

-- TODO: Move this to a shared types module
newtype Token = Token { token :: Text }
  deriving stock (Generic)

instance ToJSON Token

newtype Scenario = Scenario { getScenario :: Text }
  deriving newtype (ToJSON)

newtype Cycle = Cycle { getCycle :: Text }
  deriving newtype (Eq, Ord, ToJSON, ToJSONKey)

data GameSettings = GameSettings
    { gameCycleId    :: ArkhamHorrorCycleId
    , gameScenarioId :: ArkhamHorrorScenarioId
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

newtype CampaignSettings = CampaignSettings
  { campaignCycleId :: ArkhamHorrorCycleId
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

instance FromJSON CampaignSettings where
  parseJSON =
    withObject "CampaignSettings" $ \v -> CampaignSettings <$> v .: "cycleId"

instance FromJSON GameSettings where
  parseJSON = withObject "GameSettings"
    $ \v -> GameSettings <$> v .: "cycleId" <*> v .: "scenarioId"

instance FromJSON Cycle where
  parseJSON = withObject "Cycle" $ \v -> Cycle <$> v .: "name"

instance FromJSON Scenario where
  parseJSON = withObject "Scenario" $ \v -> Scenario <$> v .: "name"

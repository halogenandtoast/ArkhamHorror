module Api.Arkham.Export where

import Import.NoFoundation

import Api.Arkham.Types.MultiplayerVariant
import Arkham.Game
import Entity.Arkham.LogEntry
import Entity.Arkham.Step
import Json

data ArkhamExport = ArkhamExport
  { aeCampaignPlayers :: [Text]
  , aeCampaignData :: ArkhamGameExportData
  }
  deriving stock (Generic)

instance ToJSON ArkhamExport where
  toJSON = genericToJSON $ aesonOptions $ Just "ae"

instance FromJSON ArkhamExport where
  parseJSON = genericParseJSON $ aesonOptions $ Just "ae"

data ArkhamGameExportData = ArkhamGameExportData
  { agedName :: Text
  , agedCurrentData :: Game
  , agedStep :: Int
  , agedSteps :: [ArkhamStep]
  , agedLog :: [ArkhamLogEntry]
  , agedMultiplayerVariant :: MultiplayerVariant
  }
  deriving stock (Generic)

instance ToJSON ArkhamGameExportData where
  toJSON = genericToJSON $ aesonOptions $ Just "aged"

instance FromJSON ArkhamGameExportData where
  parseJSON = genericParseJSON $ aesonOptions $ Just "aged"

arkhamGameToExportData :: ArkhamGame -> [ArkhamStep] -> [ArkhamLogEntry] -> ArkhamGameExportData
arkhamGameToExportData ArkhamGame {..} steps gameLog =
  ArkhamGameExportData
    { agedName = arkhamGameName
    , agedCurrentData = arkhamGameCurrentData
    , agedStep = arkhamGameStep
    , agedSteps = steps
    , agedLog = gameLog
    , agedMultiplayerVariant = arkhamGameMultiplayerVariant
    }

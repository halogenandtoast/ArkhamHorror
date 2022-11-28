module Api.Arkham.Export where

import Import.NoFoundation

import Api.Arkham.Types.MultiplayerVariant
import Arkham.Game
import Entity.Arkham.Step
import Json

data ArkhamExport = ArkhamExport
  { aeCampaignPlayers :: [Text]
  , aeCampaignData :: ArkhamGameExportData
  }
  deriving stock Generic

instance ToJSON ArkhamExport where
  toJSON = genericToJSON (aesonOptions $ Just "ae")

instance FromJSON ArkhamExport where
  parseJSON = genericParseJSON (aesonOptions $ Just "ae")

data ArkhamGameExportData = ArkhamGameExportData
  { agedName :: Text
  , agedCurrentData :: Game
  , agedStep :: Int
  , agedSteps :: [ArkhamStep]
  , agedLog :: [Text]
  , agedMultiplayerVariant :: MultiplayerVariant
  }
  deriving stock Generic

instance ToJSON ArkhamGameExportData where
  toJSON = genericToJSON (aesonOptions $ Just "aged")

instance FromJSON ArkhamGameExportData where
  parseJSON = genericParseJSON (aesonOptions $ Just "aged")

arkhamGameToExportData :: ArkhamGame -> [ArkhamStep] -> ArkhamGameExportData
arkhamGameToExportData ArkhamGame {..} steps = ArkhamGameExportData
  { agedName = arkhamGameName
  , agedCurrentData = arkhamGameCurrentData
  , agedStep = arkhamGameStep
  , agedSteps = steps
  , agedLog = arkhamGameLog
  , agedMultiplayerVariant = arkhamGameMultiplayerVariant
  }

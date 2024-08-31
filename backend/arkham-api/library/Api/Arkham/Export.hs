module Api.Arkham.Export where

import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant
import Arkham.Game
import Database.Esqueleto.Experimental
import Entity.Arkham.LogEntry
import Entity.Arkham.Step
import Import hiding ((==.))
import Json

data ArkhamExport = ArkhamExport
  { aeCampaignPlayers :: [Text]
  , aeCampaignData :: ArkhamGameExportData
  }
  deriving stock Generic

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
  deriving stock Generic

instance ToJSON ArkhamGameExportData where
  toJSON = genericToJSON $ aesonOptions $ Just "aged"

instance FromJSON ArkhamGameExportData where
  parseJSON = genericParseJSON $ aesonOptions $ Just "aged"

arkhamGameToExportData :: ArkhamGame -> [ArkhamStep] -> [ArkhamLogEntry] -> ArkhamGameExportData
arkhamGameToExportData ArkhamGame {..} steps _gameLog =
  ArkhamGameExportData
    { agedName = arkhamGameName
    , agedCurrentData = arkhamGameCurrentData
    , agedStep = arkhamGameStep
    , agedSteps = steps
    , agedLog = []
    , agedMultiplayerVariant = arkhamGameMultiplayerVariant
    }

generateExport :: ArkhamGameId -> Handler ArkhamExport
generateExport gameId = do
  (ge, players, steps, entries) <- runDB $ do
    ge <- get404 gameId
    players <- select $ do
      players <- from $ table @ArkhamPlayer
      where_ (players ^. ArkhamPlayerArkhamGameId ==. val gameId)
      pure players
    steps <- select $ do
      steps <- from $ table @ArkhamStep
      where_ $ steps ^. ArkhamStepArkhamGameId ==. val gameId
      orderBy [desc $ steps ^. ArkhamStepStep]
      pure steps

    entries <- getGameLogEntries gameId
    pure (ge, players, steps, entries)

  pure
    $ ArkhamExport
      { aeCampaignPlayers = map (arkhamPlayerInvestigatorId . entityVal) players
      , aeCampaignData = arkhamGameToExportData ge (map entityVal steps) entries
      }

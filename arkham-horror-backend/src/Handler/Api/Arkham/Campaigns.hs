module Handler.Api.Arkham.Campaigns where

import           Import

getFirstScenario :: MonadHandler m => ArkhamHorrorCycleId -> SqlPersistT m (Entity ArkhamHorrorScenario)
getFirstScenario = getBy404 . flip ScenarioCyclePosition 1

data CampaignJson = CampaignJson
  { cycleId :: ArkhamHorrorCycleId
  , deckUrl :: Text
  } deriving stock (Generic)

instance FromJSON CampaignJson

postApiV1ArkhamCampaignsR :: Handler ArkhamHorrorGameId
postApiV1ArkhamCampaignsR = do
  currentUserId <- requireAuthId
  CampaignJson {..} <- requireCheckJsonBody
  runDB $ do
    campaignId <- insert $ ArkhamHorrorGameCampaign cycleId
    (Entity scenarioId _) <- getFirstScenario cycleId
    gameScenarioId <- insert $ ArkhamHorrorGameScenario scenarioId
    gameId         <- insert $ ArkhamHorrorGame campaignId gameScenarioId
    insert_ $ ArkhamHorrorGameInvestigator gameId currentUserId (object [ "url" .= deckUrl ])
    pure gameId

getApiV1ArkhamCampaignsCampaignR :: ArkhamHorrorGameCampaignId -> Handler ArkhamHorrorGameCampaign
getApiV1ArkhamCampaignsCampaignR = runDB . get404

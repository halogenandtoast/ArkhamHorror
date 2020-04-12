module Arkham.Handler.Api.Campaigns where

import           Import hiding (on, (==.))
import Arkham.Types
import Database.Esqueleto
import Data.Aeson
import Utility

getFirstScenario
  :: MonadHandler m
  => ArkhamHorrorCycleId
  -> SqlPersistT m (Entity ArkhamHorrorScenario)
getFirstScenario = getBy404 . flip ScenarioCyclePosition 1

data CampaignJson = CampaignJson
    { cycleId :: ArkhamHorrorCycleId
    , deckUrl :: Text
    , difficulty :: Difficulty
    }
    deriving stock (Generic)

instance FromJSON CampaignJson

postApiV1ArkhamCampaignsR :: Handler ArkhamHorrorGameId
postApiV1ArkhamCampaignsR = do
  currentUserId     <- requireAuthId
  CampaignJson {..} <- requireCheckJsonBody
  runDB $ do
    campaignId     <- insert $ ArkhamHorrorGameCampaign cycleId difficulty
    scenarioId     <- entityKey <$> getFirstScenario cycleId
    gameScenarioId <- insert $ ArkhamHorrorGameScenario scenarioId
    gameId         <- insert $ ArkhamHorrorGame campaignId gameScenarioId
    insert_ $ ArkhamHorrorGameInvestigator gameId
                                           currentUserId
                                           (object ["url" .= deckUrl])
    pure gameId

data ArkhamHorrorGameCampaignResponse = ArkhamHorrorGameCampaignResponse
  { ahgcrCycle :: Entity ArkhamHorrorCycle
  , ahgcrDifficulty :: Difficulty
  } deriving stock (Generic)

instance ToJSON ArkhamHorrorGameCampaignResponse where
  toJSON = genericToJSON $ aesonOptions $ Just "ahgcr"

getApiV1ArkhamCampaignsCampaignR
  :: ArkhamHorrorGameCampaignId -> Handler ArkhamHorrorGameCampaignResponse
getApiV1ArkhamCampaignsCampaignR campaignId = do
  mresponse <- runDB $ (headMay . map convert <$>) <$> select . from $ \(campaigns `InnerJoin` cycles) -> do
    on $ campaigns ^. ArkhamHorrorGameCampaignCycleId ==. cycles ^. persistIdField
    where_ $ campaigns ^. persistIdField ==. val campaignId
    limit 1
    pure (cycles, campaigns ^. ArkhamHorrorGameCampaignDifficulty)
  fromMaybeM notFound mresponse
 where
   convert = uncurry ArkhamHorrorGameCampaignResponse . second unValue


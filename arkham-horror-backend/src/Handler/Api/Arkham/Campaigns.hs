module Handler.Api.Arkham.Campaigns where

import Import
import Arkham.Types

postApiV1ArkhamCampaignsR :: Handler (Key ArkhamHorrorGame)
postApiV1ArkhamCampaignsR = do
  mCurrentUserId <- maybeAuthId
  case mCurrentUserId of
    Nothing -> notAuthenticated
    Just currentUserId -> do
      CampaignSettings {..} <- requireCheckJsonBody
      runDB $ do
        ahgcCampaignId <- insert $ ArkhamHorrorGameCampaign campaignCycleId
        (Entity scenarioId _) <- getBy404 $  ScenarioCyclePosition campaignCycleId 1
        ahgsId <- insert $ ArkhamHorrorGameScenario scenarioId
        ahgId <- insert $ ArkhamHorrorGame ahgcCampaignId ahgsId
        insert $ ArkhamHorrorGameInvestigator ahgId currentUserId
        pure ahgId

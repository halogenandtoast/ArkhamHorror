module Handler.Api.Arkham.Campaigns where

import           Arkham.Types
import           Import

postApiV1ArkhamCampaignsR :: Handler (Key ArkhamHorrorGame)
postApiV1ArkhamCampaignsR = do
  mCurrentUserId <- maybeAuthId
  case mCurrentUserId of
    Nothing            -> notAuthenticated
    Just currentUserId -> do
      CampaignSettings {..} <- requireCheckJsonBody
      runDB $ do
        campaignId <- insert $ ArkhamHorrorGameCampaign campaignCycleId
        (Entity scenarioId _) <- getBy404
          $ ScenarioCyclePosition campaignCycleId 1
        gameScenarioId <- insert $ ArkhamHorrorGameScenario scenarioId
        gameId         <- insert $ ArkhamHorrorGame campaignId gameScenarioId
        insert_ $ ArkhamHorrorGameInvestigator gameId currentUserId
        pure gameId

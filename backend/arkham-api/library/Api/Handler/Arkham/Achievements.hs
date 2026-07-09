module Api.Handler.Arkham.Achievements (
  getApiV1ArkhamAchievementsR,
  deleteApiV1ArkhamAchievementsR,
  getApiV1ArkhamGameAchievementsR,
) where

import Arkham.Achievement.Types (Achievement, achievementCampaigns, allAchievements)
import Data.Aeson (withObject)
import Data.Aeson.Types (parseFail)
import Import hiding ((==.))
import Import qualified as P

-- | All of the requesting user's achievement rows (earned and in-progress).
getApiV1ArkhamAchievementsR :: Handler [Entity ArkhamAchievement]
getApiV1ArkhamAchievementsR = do
  userId <- getRequestUserId
  runDB $ P.selectList [ArkhamAchievementUserId P.==. userId] [P.Asc ArkhamAchievementEarnedAt]

-- | Which earned achievements to clear. Only earned rows are touched;
-- in-progress rows (earnedAt null) keep accruing.
data ClearAchievements
  = ClearAll
  | ClearCampaign Text
  | ClearAchievement Achievement

instance FromJSON ClearAchievements where
  parseJSON = withObject "ClearAchievements" \o ->
    (o .: "scope") >>= \case
      ("all" :: Text) -> pure ClearAll
      "campaign" -> ClearCampaign <$> o .: "campaign"
      "achievement" -> ClearAchievement <$> o .: "achievement"
      other -> parseFail $ "Unknown clear scope: " <> show other

deleteApiV1ArkhamAchievementsR :: Handler ()
deleteApiV1ArkhamAchievementsR = do
  userId <- getRequestUserId
  clear <- requireCheckJsonBody
  let
    earnedBy =
      [ ArkhamAchievementUserId P.==. userId
      , ArkhamAchievementEarnedAt P.!=. Nothing
      ]
  runDB $ case clear of
    ClearAll -> P.deleteWhere earnedBy
    ClearCampaign campaign ->
      P.deleteWhere
        $ earnedBy
        <> [ ArkhamAchievementAchievement
              P.<-. filter (elem campaign . achievementCampaigns) allAchievements
           ]
    ClearAchievement achievement ->
      P.deleteWhere $ earnedBy <> [ArkhamAchievementAchievement P.==. achievement]

-- | Achievements earned in a specific game, for the campaign log tab. Any
-- player of the game may view them (same access rule as the game itself).
getApiV1ArkhamGameAchievementsR :: ArkhamGameId -> Handler [Entity ArkhamAchievement]
getApiV1ArkhamGameAchievementsR gameId = do
  userId <- getRequestUserId
  void $ runDB $ getBy404 (UniquePlayer userId gameId)
  runDB
    $ P.selectList
      [ArkhamAchievementArkhamGameId P.==. Just gameId]
      [P.Asc ArkhamAchievementEarnedAt]

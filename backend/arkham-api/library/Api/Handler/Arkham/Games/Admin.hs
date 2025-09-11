{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Api.Handler.Arkham.Games.Admin (
  getApiV1AdminR,
  getApiV1AdminGameR,
  putApiV1AdminGameR,
  getApiV1AdminGamesR,
  putApiV1AdminGameRawR,
) where

import Api.Arkham.Helpers
import Api.Handler.Arkham.Games.Shared
import Arkham.Game
import Conduit
import Data.Time.Clock
import Database.Esqueleto.Experimental hiding (update, (=.))
import Entity.Answer
import Entity.Arkham.GameRaw
import Entity.Arkham.Player
import Import hiding (delete, exists, on, (==.), (>=.))
import Yesod.WebSockets

getAdminUser :: Handler (Entity User)
getAdminUser = do
  userId <- getRequestUserId
  user <- runDB $ get404 userId
  unless user.admin $ permissionDenied "You must be an admin to access this endpoint"
  pure $ Entity userId user

data AdminData = AdminData
  { currentUsers :: Int
  , activeUsers :: Int
  , activeGames :: [GameDetailsEntry]
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

selectCount :: SqlQuery a -> DB Int
selectCount inner = fmap (sum . map unValue . toList) . selectOne $ inner $> countRows

getApiV1AdminR :: Handler AdminData
getApiV1AdminR = do
  _user <- getAdminUser
  recent <- addUTCTime (negate (14 * nominalDay)) <$> liftIO getCurrentTime
  runDB do
    currentUsers <- selectCount $ from $ table @User
    activeUsers <-
      fmap (sum . map unValue . toList) . selectOne $ do
        (users :& _players :& games) <-
          from
            $ table @User
            `innerJoin` table @ArkhamPlayer
              `on` (\(users :& players) -> users.id ==. players.userId)
            `innerJoin` table @ArkhamGame
              `on` (\(_ :& players :& games) -> players.arkhamGameId ==. games.id)
        where_ (games.updatedAt >=. val recent)
        pure (countDistinct users.id)

    AdminData currentUsers activeUsers <$> recentGames 20

getApiV1AdminGameR :: ArkhamGameId -> Handler GetGameJson
getApiV1AdminGameR gameId = do
  _user <- getAdminUser
  webSockets $ gameStream Nothing gameId
  g <- runDB $ get404 gameId
  let Game {..} = g.currentData
  gameLog <- runDB $ getGameLog gameId Nothing
  let player = gameActivePlayerId
  pure $ GetGameJson (Just player) g.variant (PublicGame gameId g.name gameLog.entries g.currentData)

recentGames :: Int64 -> DB [GameDetailsEntry]
recentGames n = do
  games <- select do
    games <- from $ table @ArkhamGameRaw
    orderBy [desc games.updatedAt]
    limit n
    pure games
  pure $ map toGameDetailsEntry games

getApiV1AdminGamesR :: Handler [GameDetailsEntry]
getApiV1AdminGamesR = do
  _user <- getAdminUser
  runDB $ recentGames 20

putApiV1AdminGameR :: ArkhamGameId -> Handler ()
putApiV1AdminGameR gameId = do
  userId <- getRequestUserId
  response <- requireCheckJsonBody
  writeChannel <- (.channel) <$> getRoom gameId
  updateGame response gameId userId writeChannel

-- TODO: Make this a websocket message
putApiV1AdminGameRawR :: ArkhamGameId -> Handler ()
putApiV1AdminGameRawR gameId = do
  Entity userId _ <- getAdminUser
  response <- requireCheckJsonBody @_ @RawGameJsonPut
  writeChannel <- (.channel) <$> getRoom gameId
  updateGame (Raw response.gameMessage) gameId userId writeChannel

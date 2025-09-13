{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Api.Handler.Arkham.Games (
  getApiV1ArkhamGameR,
  getApiV1ArkhamGameSpectateR,
  getApiV1ArkhamGamesR,
  postApiV1ArkhamGamesR,
  putApiV1ArkhamGameR,
  deleteApiV1ArkhamGameR,
  putApiV1ArkhamGameRawR,
) where

import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant
import Api.Handler.Arkham.Games.Shared
import Arkham.Classes.HasQueue
import Arkham.Difficulty
import Arkham.Game
import Arkham.Id
import Arkham.Queue
import Conduit
import Control.Monad.Random (mkStdGen)
import Control.Monad.Random.Class (getRandom)
import Data.Coerce
import Data.Time.Clock
import Database.Esqueleto.Experimental hiding (update, (=.))
import Entity.Answer
import Entity.Arkham.GameRaw
import Entity.Arkham.Step
import Import hiding (delete, exists, on, (==.))
import Yesod.WebSockets

getApiV1ArkhamGameR :: ArkhamGameId -> Handler GetGameJson
getApiV1ArkhamGameR gameId = do
  userId <- getRequestUserId
  webSockets $ gameStream (Just userId) gameId
  runDB do
    g <- get404 gameId
    gameLog <- getGameLog gameId Nothing
    Entity playerId _ <- getBy404 (UniquePlayer userId gameId)
    let Game {..} = g.currentData
    let
      player =
        case g.variant of
          WithFriends -> coerce playerId
          Solo -> gameActivePlayerId
    pure $ GetGameJson (Just player) g.variant (PublicGame gameId g.name gameLog.entries g.currentData)

getApiV1ArkhamGameSpectateR :: ArkhamGameId -> Handler GetGameJson
getApiV1ArkhamGameSpectateR gameId = do
  webSockets $ gameStream Nothing gameId
  runDB do
    g <- get404 gameId
    let Game {..} = g.currentData
    gameLog <- getGameLog gameId Nothing
    let player = gameActivePlayerId
    pure $ GetGameJson (Just player) g.variant (PublicGame gameId g.name gameLog.entries g.currentData)

getApiV1ArkhamGamesR :: Handler [GameDetailsEntry]
getApiV1ArkhamGamesR = do
  userId <- getRequestUserId
  games <- runDB $ select do
    (players :& games) <-
      distinct
        $ from
        $ table @ArkhamPlayer
        `innerJoin` table @ArkhamGameRaw
          `on` (\(players :& games) -> players.arkhamGameId ==. toBaseId games.id)
    where_ $ players.userId ==. val userId
    orderBy [desc games.updatedAt]
    pure games

  pure $ map toGameDetailsEntry games

data CreateGamePost = CreateGamePost
  { deckIds :: [Maybe ArkhamDeckId]
  , playerCount :: Int
  , campaignId :: Maybe CampaignId
  , scenarioId :: Maybe ScenarioId
  , difficulty :: Difficulty
  , campaignName :: Text
  , multiplayerVariant :: MultiplayerVariant
  , includeTarotReadings :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

-- | New Game
postApiV1ArkhamGamesR :: Handler (PublicGame ArkhamGameId)
postApiV1ArkhamGamesR = do
  userId <- getRequestUserId
  CreateGamePost {..} <- requireCheckJsonBody
  newGameSeed <- liftIO getRandom
  genRef <- newIORef (mkStdGen newGameSeed)
  queueRef <- newQueue []
  now <- liftIO getCurrentTime

  let
    game = case campaignId of
      Just cid -> newCampaign cid scenarioId newGameSeed playerCount difficulty includeTarotReadings
      Nothing -> case scenarioId of
        Just sid -> newScenario sid newGameSeed playerCount difficulty includeTarotReadings
        Nothing -> error "missing either a campign id or a scenario id"
    ag = ArkhamGame campaignName game 0 multiplayerVariant now now
    repeatCount = if multiplayerVariant == WithFriends then 1 else playerCount

  runDB do
    gameId <- insert ag
    pids <- replicateM repeatCount $ insert $ ArkhamPlayer userId gameId "00000"
    gameRef <- liftIO $ newIORef game

    runGameApp (GameApp gameRef queueRef genRef (pure . const ())) do
      for_ pids \pid -> addPlayer (PlayerId $ coerce pid)
      runMessages Nothing

    updatedQueue <- liftIO $ readIORef (queueToRef queueRef)
    updatedGame <- liftIO $ readIORef gameRef

    let ag' = ag {arkhamGameCurrentData = updatedGame}

    replace gameId ag'
    insert_ $ ArkhamStep gameId (Choice mempty updatedQueue) 0 (ActionDiff [])
    pure $ toPublicGame (Entity gameId ag') mempty

putApiV1ArkhamGameR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameR gameId = do
  Entity userId user <- getRequestUser
  unless user.admin do
    void $ runDB $ getBy404 (UniquePlayer userId gameId)
  response <- requireCheckJsonBody
  writeChannel <- (.channel) <$> getRoom gameId
  updateGame response gameId userId writeChannel

-- TODO: Make this a websocket message
putApiV1ArkhamGameRawR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameRawR gameId = do
  userId <- getRequestUserId
  response <- requireCheckJsonBody @_ @RawGameJsonPut
  writeChannel <- (.channel) <$> getRoom gameId
  updateGame (Raw response.gameMessage) gameId userId writeChannel

deleteApiV1ArkhamGameR :: ArkhamGameId -> Handler ()
deleteApiV1ArkhamGameR gameId = do
  userId <- getRequestUserId
  runDB $ delete do
    games <- from $ table @ArkhamGame
    where_ $ games.id ==. val gameId
    where_ $ exists do
      players <- from $ table @ArkhamPlayer
      where_ $ players.arkhamGameId ==. games.id
      where_ $ players.userId ==. val userId

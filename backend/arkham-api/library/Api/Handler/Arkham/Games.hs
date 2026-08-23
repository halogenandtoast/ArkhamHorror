{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Api.Handler.Arkham.Games (
  getApiV1ArkhamGameR,
  getApiV1ArkhamGameSpectateR,
  getApiV1ArkhamGameStepR,
  getApiV1ArkhamGamesR,
  postApiV1ArkhamGamesR,
  putApiV1ArkhamGameR,
  deleteApiV1ArkhamGameR,
  putApiV1ArkhamGameRawR,
  postApiV1ArkhamGamePlayabilityR,
) where

import Api.Arkham.Epic (lookupGameEvent)
import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant
import Api.Handler.Arkham.Games.Shared
import Arkham.Campaign.Option
import Arkham.Card
import Arkham.Classes.HasQueue
import Arkham.Cost.Status
import Arkham.Difficulty
import Arkham.Game
import Arkham.Game.Settings (
  AsIfRuling,
  asIfRulingFromStrictAsIfAt,
  defaultAsIfRulingForCampaign,
  settingsAchievementsEnabled,
  settingsAsIfRuling,
  settingsUltimatumsAndBoons,
 )
import Arkham.GameEnv (getCard)
import Arkham.Helpers.Playable (getPlayabilityChecks)
import Arkham.Id
import Arkham.Message (Message (HandleOption))
import Arkham.Queue
import Arkham.Source
import Arkham.UltimatumsAndBoons.Types (UltimatumOrBoon)
import Arkham.Window (mkWhen)
import Arkham.Window qualified as Window
import Conduit
import Control.Monad.Random (mkStdGen)
import Control.Monad.Random.Class (getRandom)
import Data.Aeson (withObject, (.!=), (.:?))
import Data.Coerce
import Data.Map.Strict qualified as Map
import Data.Time.Clock
import Database.Esqueleto.Experimental hiding (update, (=.))
import Entity.Answer
import Entity.Arkham.GameRaw
import Entity.Arkham.Step
import Import hiding (delete, exists, on, (==.))
import Yesod.WebSockets

{- | Step counter only: one indexed Int column, no game JSON touched, no lock.

The full game GET is the most expensive endpoint we have (whole game plus log),
so nothing that merely wants to know "did anything happen?" should be hitting it
on a timer. Poll this instead and fetch the game only when the step moved.
-}
getApiV1ArkhamGameStepR :: ArkhamGameId -> Handler GameStepJson
getApiV1ArkhamGameStepR gameId = do
  void getRequestUserId
  steps <- runDB $ select do
    games <- from $ table @ArkhamGame
    where_ $ games.id ==. val gameId
    pure games.step
  case steps of
    Value step : _ -> pure $ GameStepJson step
    [] -> notFound

newtype GameStepJson = GameStepJson
  { step :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

getApiV1ArkhamGameR :: ArkhamGameId -> Handler GetGameJson
getApiV1ArkhamGameR gameId = do
  userId <- getRequestUserId
  wsOptions <- websocketConnectionOptions
  webSocketsOptions wsOptions $ gameStream gameId
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
    mEvt <- lookupGameEvent gameId
    pure
      $ GetGameJson
        (Just player)
        g.variant
        (PublicGame gameId g.name gameLog.entries g.currentData)
        (entityKey . fst <$> mEvt)

getApiV1ArkhamGameSpectateR :: ArkhamGameId -> Handler GetGameJson
getApiV1ArkhamGameSpectateR gameId = do
  wsOptions <- websocketConnectionOptions
  webSocketsOptions wsOptions $ gameStream gameId
  runDB do
    g <- get404 gameId
    let Game {..} = g.currentData
    gameLog <- getGameLog gameId Nothing
    let player = gameActivePlayerId
    mEvt <- lookupGameEvent gameId
    pure
      $ GetGameJson
        (Just player)
        g.variant
        (PublicGame gameId g.name gameLog.entries g.currentData)
        (entityKey . fst <$> mEvt)

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
    -- Epic Multiplayer group games are surfaced through their event (one
    -- entry), not as standalone games in this list.
    where_ $ notExists $ do
      grp <- from $ table @ArkhamEpicGroup
      where_ $ grp.arkhamGameId ==. just (toBaseId games.id)
    orderBy [desc games.updatedAt]
    pure games
  let gameIds = map (coerce . entityKey) games :: [ArkhamGameId]
  playerCounts <- runDB $ select do
    p <- from $ table @ArkhamPlayer
    where_ $ p.arkhamGameId `in_` valList gameIds
    groupBy p.arkhamGameId
    pure (p.arkhamGameId, countRows @Int)
  let countMap = Map.fromList [(gid, n) | (Value gid, Value n) <- playerCounts]
  pure
    $ map (\g -> toGameDetailsEntry g (fromMaybe 0 $ Map.lookup (coerce $ entityKey g) countMap)) games

data CreateGamePost = CreateGamePost
  { deckIds :: [Maybe ArkhamDeckId]
  , playerCount :: Int
  , campaignId :: Maybe CampaignId
  , scenarioId :: Maybe ScenarioId
  , difficulty :: Difficulty
  , campaignName :: Text
  , multiplayerVariant :: MultiplayerVariant
  , includeTarotReadings :: Bool
  , options :: Set CampaignOption
  , strictAsIfAt :: Maybe Bool
  , asIfRuling :: Maybe AsIfRuling
  , ultimatumsAndBoons :: Set UltimatumOrBoon
  , achievementsEnabled :: Bool
  }
  deriving stock (Show, Generic)

{- | Hand-written so 'Maybe' fields stay optional and everything else stays
required.
-}
instance FromJSON CreateGamePost where
  parseJSON = withObject "CreateGamePost" \o -> do
    deckIds <- o .: "deckIds"
    playerCount <- o .: "playerCount"
    campaignId <- o .:? "campaignId"
    scenarioId <- o .:? "scenarioId"
    difficulty <- o .: "difficulty"
    campaignName <- o .: "campaignName"
    multiplayerVariant <- o .: "multiplayerVariant"
    includeTarotReadings <- o .: "includeTarotReadings"
    options <- o .: "options"
    strictAsIfAt <- o .:? "strictAsIfAt"
    asIfRuling <- o .:? "asIfRuling"
    ultimatumsAndBoons <- o .:? "ultimatumsAndBoons" .!= mempty
    achievementsEnabled <- o .:? "achievementsEnabled" .!= True
    pure CreateGamePost {..}

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
    defaultAsIfRuling = defaultAsIfRulingForCampaign $ case campaignId of
      Just (CampaignId cid) -> Just cid
      Nothing -> Nothing
    asIfRulingValue = fromMaybe defaultAsIfRuling $ asIfRuling <|> fmap asIfRulingFromStrictAsIfAt strictAsIfAt
    baseGame = case campaignId of
      Just cid -> newCampaign cid scenarioId newGameSeed playerCount difficulty includeTarotReadings
      Nothing -> case scenarioId of
        Just sid -> newScenario sid newGameSeed playerCount difficulty includeTarotReadings
        Nothing -> error "missing either a campign id or a scenario id"
    game =
      baseGame
        { gameSettings =
            baseGame.gameSettings
              { settingsAsIfRuling = asIfRulingValue
              , settingsUltimatumsAndBoons = ultimatumsAndBoons
              , settingsAchievementsEnabled = achievementsEnabled
              }
        }
    ag = ArkhamGame campaignName game 0 multiplayerVariant now now
    repeatCount = if multiplayerVariant == WithFriends then 1 else playerCount

  runDB do
    gameId <- insert ag
    pids <- replicateM repeatCount $ insert $ ArkhamPlayer userId gameId "00000"
    gameRef <- liftIO $ newIORef game

    runGameApp (GameApp gameRef queueRef genRef (pure . const ()) Nothing) do
      for_ pids \pid -> addPlayer (PlayerId $ coerce pid)
      traverse_ (push . HandleOption) (toList options)
      runMessages (gameIdToText gameId) Nothing

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
  mRoom <- lookupRoom gameId
  updateGame response gameId mRoom

-- TODO: Make this a websocket message
putApiV1ArkhamGameRawR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameRawR gameId = do
  Entity userId user <- getRequestUser
  unless user.admin do
    void $ runDB $ getBy404 (UniquePlayer userId gameId)
  response <- requireCheckJsonBody @_ @RawGameJsonPut
  mRoom <- lookupRoom gameId
  updateGame (Raw response.gameMessage) gameId mRoom

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
  deleteRoom gameId

data PlayabilityRequest = PlayabilityRequest
  { investigatorId :: InvestigatorId
  , cardId :: CardId
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

data PlayabilityResponse = PlayabilityResponse
  { cardId :: CardId
  , cardCode :: Text
  , checks :: [(Text, Maybe Text)]
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

postApiV1ArkhamGamePlayabilityR :: ArkhamGameId -> Handler PlayabilityResponse
postApiV1ArkhamGamePlayabilityR gameId = do
  userId <- getRequestUserId
  void $ runDB $ getBy404 (UniquePlayer userId gameId)
  PlayabilityRequest {investigatorId = iid, cardId = cid} <- requireCheckJsonBody
  g <- runDB $ get404 gameId
  let gameJson = g.currentData
  gameRef <- newIORef gameJson
  queueRef <- newQueue []
  genRef <- newIORef $ mkStdGen gameJson.gameSeed
  runGameApp (GameApp gameRef queueRef genRef (pure . const ()) Nothing) do
    card <- getCard cid
    let duringTurnWindows = [mkWhen (Window.DuringTurn iid)]
    checks <- getPlayabilityChecks iid (toSource iid) (UnpaidCost NeedsAction) duringTurnWindows card
    pure
      PlayabilityResponse
        { cardId = cid
        , cardCode = unCardCode (toCardCode card)
        , checks
        }

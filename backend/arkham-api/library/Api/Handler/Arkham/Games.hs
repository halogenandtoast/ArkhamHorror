{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Api.Handler.Arkham.Games (
  getApiV1ArkhamGameR,
  getApiV1ArkhamGameSpectateR,
  getApiV1ArkhamGameLogBeforeR,
  getApiV1ArkhamGameStepR,
  getApiV1ArkhamGamesR,
  getApiV1ArkhamSpectatableGamesR,
  getApiV1ArkhamSpectatableGamesDigestR,
  SpectatableGameActivity (..),
  postApiV1ArkhamGamesR,
  putApiV1ArkhamGameR,
  deleteApiV1ArkhamGameR,
  putApiV1ArkhamGameRawR,
  postApiV1ArkhamGamePlayabilityR,
) where

import Api.Arkham.Epic (lookupGameEvent)
import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant
import Api.Handler.Arkham.CustomCards (userCustomCards)
import Api.Handler.Arkham.Games.Shared
import Api.Handler.Arkham.Games.Summary (
  ownGameSummaries,
  spectatableGameSummaries,
  spectatableGamesLimit,
 )
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
import Arkham.Game.State (GameState (IsActive))
import Arkham.GameEnv (getCard)
import Arkham.Helpers.Playable (getPlayabilityChecks)
import Arkham.Id
import Arkham.Log.Entry (LogRow)
import Arkham.Message (Message (ChatMessage, HandleOption))
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
import Data.Set qualified as Set
import Data.Time.Clock
import Database.Esqueleto.Experimental hiding (update, (=.))
import Database.Esqueleto.Internal.Internal (unsafeSqlBinOp, unsafeSqlValue)
import Entity.Answer
import Entity.Arkham.GameRaw
import Entity.Arkham.Step
import Import hiding (Value, delete, exists, on, (==.))

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
  servePongAware $ gameStream gameId
  runDB do
    g <- get404 gameId
    gameLog <- getGameLogTail gameId gameLogTailSize
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

{- | A page of scrollback: the rows immediately older than @before@.

The game payload carries only the newest 'gameLogTailSize' entries, so this is
how a reader reaches the rest. Membership is checked the same way the game read
is, because the log can carry entries addressed to one seat.
-}
getApiV1ArkhamGameLogBeforeR :: ArkhamGameId -> Int -> Handler [LogRow]
getApiV1ArkhamGameLogBeforeR gameId before = do
  userId <- getRequestUserId
  runDB do
    void $ getBy404 (UniquePlayer userId gameId)
    (.entries) <$> getGameLogBefore gameId before gameLogTailSize

{- | Watch a game without a seat in it.

Reaching this needs only the game's id, as it always has -- a link somebody
shared still works. @allowSpectators@ governs whether the game is *advertised*
on the home page's watch list, not whether this read is allowed.

The socket is 'spectatorStream', so a watcher cannot answer the game's
questions.
-}
getApiV1ArkhamGameSpectateR :: ArkhamGameId -> Handler GetGameJson
getApiV1ArkhamGameSpectateR gameId = do
  servePongAware $ spectatorStream gameId
  runDB do
    g <- get404 gameId
    let Game {..} = g.currentData
    gameLog <- getGameLogTail gameId gameLogTailSize
    let player = gameActivePlayerId
    mEvt <- lookupGameEvent gameId
    pure
      $ GetGameJson
        (Just player)
        g.variant
        (PublicGame gameId g.name gameLog.entries g.currentData)
        (entityKey . fst <$> mEvt)

{- | The home page's list.

Reads a projection of each game rather than the game -- see
"Api.Handler.Arkham.Games.Summary" for why, and for what it costs. Epic
Multiplayer group games are surfaced through their event (one entry), not as
standalone games here, which the query takes care of.
-}
getApiV1ArkhamGamesR :: Handler [GameDetailsEntry]
getApiV1ArkhamGamesR = do
  userId <- getRequestUserId
  runDB $ ownGameSummaries userId

{- | The rows the watch list's digest is built from: games whose players have
opened them up to be watched.

Deliberately not every game this user *could* reach: @allowSpectators@ is what
puts a game here, and it is decided when the game is created and never changed.
Three things are left out --

  * games this user already has a seat in, which "Your Games" lists;
  * Epic Multiplayer group games, which are reached through their event the
    same way they are kept out of 'getApiV1ArkhamGamesR';
  * anything not actually in play, so a watcher never opens a game that is
    waiting on its decks or already over.

The same four conditions the list itself applies, in
'Api.Handler.Arkham.Games.Summary.spectatableGameSummaries'. They have to stay
the same question: a digest answering "has the list changed?" about some other
list is worse than no digest.

What this query cannot answer is whether anybody is *there*. @IsActive@ is the
game state -- not pending decks, not over -- and an abandoned game keeps it
forever, so an advertised game nobody has opened in days still matched every
condition above. Presence lives in the rooms (Redis, or this pod's map), which
is a different store entirely, so both callers intersect these rows with
'getLiveGameRooms'.

The limit is applied before that intersection, which cannot lose a live game in
practice: the order is @updatedAt@ descending and a game somebody is connected
to is a game being played, so it sorts above the abandoned ones.
-}
spectatableGames :: UserId -> SqlQuery (SqlExpr (Entity ArkhamGameRaw))
spectatableGames userId = do
  games <- from $ table @ArkhamGameRaw
  where_ games.allowSpectators
  where_ $ isActiveGame games
  where_ $ notExists do
    players <- from $ table @ArkhamPlayer
    where_ $ players.arkhamGameId ==. toBaseId games.id
    where_ $ players.userId ==. val userId
  where_ $ notExists do
    grp <- from $ table @ArkhamEpicGroup
    where_ $ grp.arkhamGameId ==. just (toBaseId games.id)
  orderBy [desc games.updatedAt]
  limit spectatableGamesLimit
  pure games

{- | @current_data #>> '{gameGameState,tag}' = 'IsActive'@.

Whether play has stopped lives inside @current_data@ and nowhere else, so the
alternative is decoding every candidate game to find out -- 28kB of jsonb each.
Postgres reads the one path instead, at about 0.7ms a row, which is what makes
the digest cheap enough to poll. Same path the @arkham_game_stats@ view reads.

Only ever paid for a row @allowSpectators@ already let through: Postgres orders
the cheap boolean qual ahead of this one in the same scan.
-}
isActiveGame :: SqlExpr (Entity ArkhamGameRaw) -> SqlExpr (Value Bool)
isActiveGame games =
  unsafeSqlBinOp "#>>" games.currentData (unsafeSqlValue "'{gameGameState,tag}'")
    ==. val (Just ("IsActive" :: Text))

{- | Games whose players have opened them up to be watched.

A projection per row rather than a game apiece, so this is no longer the read
that had to be kept off a timer; the watch page still polls
'getApiV1ArkhamSpectatableGamesDigestR' and pays for the list only when the set
of games moves, which is a smaller answer either way.
-}
getApiV1ArkhamSpectatableGamesR :: Handler [GameDetailsEntry]
getApiV1ArkhamSpectatableGamesR = do
  userId <- getRequestUserId
  entries <- runDB $ spectatableGameSummaries userId
  live <- getLiveGameRooms
  pure $ filter (beingPlayed live) entries
 where
  {- The query already asked Postgres for games in play; this drops the rows
  that failed to decode, which have no state to go on either way, and the ones
  with nobody connected -- "in play" is a game state and says nothing about
  whether anybody is at the table. -}
  beingPlayed live = \case
    SuccessGameDetails gd -> gd.gameState == IsActive && gd.id `Set.member` live
    FailedGameDetails {} -> False

{- | Which games the watch list would show.

The list itself is too expensive to sit on a timer, so the watch page asks this
instead and pays for the list only when the answer changes -- the same bargain
'getApiV1ArkhamGameStepR' strikes for a game. Nothing here reads a game: it is
the membership query returning keys, so it stays a handful of uuids however
many games are open.

Sorted by id, so membership is the whole of what the client compares. The list
is ordered by @updatedAt@, which every action in a watched game moves, and a
digest reporting that order would buy the whole list again every time anybody
took a turn. A game appearing or ending is what a watcher waiting for a table
needs to see; two of them trading places in the list is not.

Ids rather than a hash, because the client only needs to know *that* the set
moved and a hash of 50 uuids is not meaningfully smaller than the uuids.

Each id carries 'idleSeconds' alongside it -- see 'SpectatableGameActivity' --
which is the one thing besides membership a watcher cannot work out from the
list it already has.
-}
getApiV1ArkhamSpectatableGamesDigestR :: Handler [SpectatableGameActivity]
getApiV1ArkhamSpectatableGamesDigestR = do
  userId <- getRequestUserId
  rows <- runDB $ select do
    games <- spectatableGames userId
    pure (games.id, games.updatedAt)
  -- The same presence filter the list applies, for the same reason.
  live <- getLiveGameRooms
  now <- liftIO getCurrentTime
  pure
    $ sortOn
      (.id)
      [ activity now gid (unValue updatedAt)
      | (gameId, updatedAt) <- rows
      , let gid = coerce (unValue gameId)
      , gid `Set.member` live
      ]
 where
  activity now gid updatedAt =
    SpectatableGameActivity
      { id = gid
      , idleSeconds = max 0 $ round $ nominalDiffTimeToSeconds $ diffUTCTime now updatedAt
      }

{- | A game on the watch list, and how long it has been quiet.

@updatedAt@ moves on every action in a game, so the gap between it and now is
how long nobody at that table has done anything -- which is how the watch page
tells a game somebody is playing from one whose players wandered off, both of
which are @IsActive@ and look identical in the list.

Elapsed seconds rather than the timestamp itself, so the answer does not depend
on the watcher's clock agreeing with the server's.
-}
data SpectatableGameActivity = SpectatableGameActivity
  { id :: ArkhamGameId
  , idleSeconds :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

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
  , allowSpectators :: Bool
  {- ^ list this game on the home page for anyone to watch. Off unless asked
  for, which is also what the account default starts as. Decided here and only
  here: nothing changes it once the game exists.
  -}
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
    allowSpectators <- o .:? "allowSpectators" .!= False
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
    ag = ArkhamGame campaignName game 0 multiplayerVariant now now allowSpectators
    repeatCount = if multiplayerVariant == WithFriends then 1 else playerCount

  runDB do
    gameId <- insert ag
    pids <- replicateM repeatCount $ insert $ ArkhamPlayer userId gameId "00000"
    gameRef <- liftIO $ newIORef game

    runGameApp (GameApp gameRef queueRef genRef (pure . const ()) Nothing) do
      for_ pids \pid -> addPlayer (PlayerId $ coerce pid)
      traverse_ (push . HandleOption) (toList options)
      runMessages (gameIdToText gameId) noRunObservers

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
  -- A game's saved copy lets it survive the author deleting a card, while the
  -- owner's library is the live definition. `updateGame` registers these after
  -- parsing the game, so a saved edit wins for this request too.
  customCards <- userCustomCards userId
  response <- requireCheckJsonBody
  mRoom <- lookupRoom gameId
  updateGame customCards response gameId mRoom

-- TODO: Make this a websocket message
putApiV1ArkhamGameRawR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameRawR gameId = do
  Entity userId user <- getRequestUser
  unless user.admin do
    void $ runDB $ getBy404 (UniquePlayer userId gameId)
  customCards <- userCustomCards userId
  response <- requireCheckJsonBody @_ @RawGameJsonPut
  mRoom <- lookupRoom gameId
  {- Stamp who is speaking from the session, not from the body: the client sends
  the investigator and the words, and the name on the line is the authenticated
  account's, so a line cannot be signed with somebody else's name. -}
  let
    attributed = case response.gameMessage of
      ChatMessage iid _ text -> ChatMessage iid (Just user.username) text
      other -> other
  updateGame customCards (Raw attributed) gameId mRoom

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

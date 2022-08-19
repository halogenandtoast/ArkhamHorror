module Api.Handler.Arkham.PendingGames
  ( putApiV1ArkhamPendingGameR
  ) where

import Import hiding ( on, (==.) )

import Api.Arkham.Helpers
import Arkham.Card.CardCode
import Arkham.Game
import Arkham.Id
import Arkham.Investigator
import Control.Monad.Random ( mkStdGen )
import Data.Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.Time.Clock
import Safe ( fromJustNote )

newtype JoinGameJson = JoinGameJson { deckId :: ArkhamDeckId }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

putApiV1ArkhamPendingGameR :: ArkhamGameId -> Handler (PublicGame ArkhamGameId)
putApiV1ArkhamPendingGameR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  JoinGameJson {..} <- requireCheckJsonBody
  ArkhamGame {..} <- runDB $ get404 gameId

  deck <- runDB $ get404 deckId
  when (arkhamDeckUserId deck /= userId) notFound
  (iid, decklist) <- liftIO $ loadDecklist deck
  when (iid `HashMap.member` gameInvestigators arkhamGameCurrentData)
    $ invalidArgs ["Investigator already taken"]

  runDB $ insert_ $ ArkhamPlayer userId gameId (coerce iid)

  let currentQueue = maybe [] choiceMessages $ headMay arkhamGameChoices

  gameRef <- newIORef arkhamGameCurrentData
  queueRef <- newIORef currentQueue
  genRef <- newIORef (mkStdGen (gameSeed arkhamGameCurrentData))
  runGameApp (GameApp gameRef queueRef genRef (pure . const ())) $ do
    addInvestigator (lookupInvestigator iid) decklist
    runMessages Nothing

  updatedGame <- readIORef gameRef
  updatedQueue <- readIORef queueRef
  let updatedMessages = []

  writeChannel <- getChannel gameId
  atomically $ writeTChan writeChannel $ encode $ GameUpdate $ PublicGame
    gameId
    arkhamGameName
    updatedMessages
    updatedGame

  now <- liftIO getCurrentTime

  runDB $ replace gameId $ ArkhamGame
    arkhamGameName
    updatedGame
    (Choice mempty mempty updatedQueue : arkhamGameChoices)
    updatedMessages
    arkhamGameMultiplayerVariant
    arkhamGameCreatedAt
    now

  pure $ toPublicGame $ Entity gameId $ ArkhamGame
    arkhamGameName
    updatedGame
    (Choice mempty mempty updatedQueue : arkhamGameChoices)
    updatedMessages
    arkhamGameMultiplayerVariant
    arkhamGameCreatedAt
    now

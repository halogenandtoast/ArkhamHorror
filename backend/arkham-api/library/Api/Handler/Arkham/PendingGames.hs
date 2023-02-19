module Api.Handler.Arkham.PendingGames
  ( putApiV1ArkhamPendingGameR
  ) where

import Import hiding ( on, (==.) )

import Api.Arkham.Helpers
import Arkham.Card.CardCode
import Arkham.Classes.HasQueue
import Arkham.Game
import Arkham.Id
import Arkham.Investigator
import Control.Monad.Random ( mkStdGen )
import Data.Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.Time.Clock
import Entity.Arkham.Step
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

  mLastStep <- runDB $ getBy (UniqueStep gameId arkhamGameStep)
  let
    currentQueue =
      maybe [] (choiceMessages . arkhamStepChoice . entityVal) mLastStep

  gameRef <- newIORef arkhamGameCurrentData
  queueRef <- newQueue currentQueue
  genRef <- newIORef (mkStdGen (gameSeed arkhamGameCurrentData))
  runGameApp (GameApp gameRef queueRef genRef (pure . const ())) $ do
    addInvestigator (lookupInvestigator iid) decklist
    runMessages Nothing

  updatedGame <- readIORef gameRef
  updatedQueue <- readIORef (queueToRef queueRef)
  let updatedMessages = []

  writeChannel <- getChannel gameId
  atomically $ writeTChan writeChannel $ encode $ GameUpdate $ PublicGame
    gameId
    arkhamGameName
    updatedMessages
    updatedGame

  now <- liftIO getCurrentTime

  runDB $ do
    replace gameId $ ArkhamGame
      arkhamGameName
      updatedGame
      (arkhamGameStep + 1)
      updatedMessages
      arkhamGameMultiplayerVariant
      arkhamGameCreatedAt
      now
    insert_
      $ ArkhamStep gameId (Choice mempty updatedQueue) (arkhamGameStep + 1)

  pure $ toPublicGame $ Entity gameId $ ArkhamGame
    arkhamGameName
    updatedGame
    (arkhamGameStep + 1)
    updatedMessages
    arkhamGameMultiplayerVariant
    arkhamGameCreatedAt
    now

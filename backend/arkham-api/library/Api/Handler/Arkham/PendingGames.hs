module Api.Handler.Arkham.PendingGames
  ( putApiV1ArkhamPendingGameR
  )
where

import Import hiding (on, (==.))

import Api.Arkham.Helpers
import Arkham.Game
import Arkham.Types.Game
import Arkham.Types.Investigator
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Database.Esqueleto
import Safe (fromJustNote)

newtype JoinGameJson = JoinGameJson { deckId :: ArkhamDeckId }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

putApiV1ArkhamPendingGameR :: ArkhamGameId -> Handler (Entity ArkhamGame)
putApiV1ArkhamPendingGameR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  let userId' = fromIntegral (fromSqlKey userId)
  JoinGameJson {..} <- requireCheckJsonBody
  ArkhamGame {..} <- runDB $ get404 gameId
  when (userId' `HashMap.member` gamePlayers arkhamGameCurrentData)
    $ invalidArgs ["Already joined game"]

  deck <- runDB $ get404 deckId
  when (arkhamDeckUserId deck /= userId) notFound
  (iid, decklist) <- liftIO $ loadDecklist deck
  when (iid `HashMap.member` gameInvestigators arkhamGameCurrentData)
    $ invalidArgs ["Investigator already taken"]

  gameRef <- newIORef arkhamGameCurrentData
  queueRef <- newIORef arkhamGameQueue
  runGameApp (GameApp gameRef queueRef) $ do
    addInvestigator userId' (lookupInvestigator iid) decklist

  runDB $ insert_ $ ArkhamPlayer userId gameId

  updatedGame <- readIORef gameRef
  updatedQueue <- readIORef queueRef
  let updatedMessages = []

  writeChannel <- getChannel gameId
  liftIO $ atomically $ writeTChan
    writeChannel
    (encode
      (Entity
        gameId
        (ArkhamGame arkhamGameName updatedGame updatedQueue updatedMessages)
      )
    )

  Entity
      gameId
      (ArkhamGame arkhamGameName updatedGame updatedQueue updatedMessages)
    <$ runDB
         (replace
           gameId
           (ArkhamGame arkhamGameName updatedGame updatedQueue updatedMessages)
         )

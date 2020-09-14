module Api.Handler.Arkham.PendingGames
  ( putApiV1ArkhamPendingGameR
  )
where

import Api.Arkham.Helpers
import Arkham.Types.Game
import Arkham.Types.Investigator
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Database.Esqueleto
import Entity.Arkham.Player
import Import hiding (on, (==.))
import Safe (fromJustNote)

newtype JoinGameJson = JoinGameJson { deckId :: String }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

putApiV1ArkhamPendingGameR :: ArkhamGameId -> Handler (Entity ArkhamGame)
putApiV1ArkhamPendingGameR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  let userId' = fromIntegral (fromSqlKey userId)
  JoinGameJson {..} <- requireCheckJsonBody
  ArkhamGame {..} <- runDB $ get404 gameId
  when (userId' `HashMap.member` _gamePlayers arkhamGameCurrentData)
    $ invalidArgs ["Already joined game"]
  (iid, deck) <- liftIO $ loadDeck deckId
  when (iid `HashMap.member` _gameInvestigators arkhamGameCurrentData)
    $ invalidArgs ["Investigator already taken"]
  ge <-
    liftIO
    $ addInvestigator userId' (lookupInvestigator iid) deck
    =<< toInternalGame arkhamGameCurrentData
  runDB $ insert_ $ ArkhamPlayer userId gameId
  writeChannel <- getChannel gameId
  liftIO $ atomically $ writeTChan
    writeChannel
    (encode (Entity gameId (ArkhamGame arkhamGameName ge)))
  Entity gameId (ArkhamGame arkhamGameName ge)
    <$ runDB (replace gameId (ArkhamGame arkhamGameName ge))

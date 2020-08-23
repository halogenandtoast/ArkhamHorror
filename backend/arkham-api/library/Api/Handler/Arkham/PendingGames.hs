module Api.Handler.Arkham.PendingGames
  ( getApiV1ArkhamPendingGameR
  , putApiV1ArkhamPendingGameR
  )
where

import Api.Arkham.Helpers
import Arkham.Types.Game
import Arkham.Types.Investigator
import Data.Aeson
import Database.Esqueleto
import Entity.Arkham.Player
import Import hiding (fromString, on, (==.))
import Safe (fromJustNote)

getApiV1ArkhamPendingGameR :: ArkhamGameId -> Handler (Entity ArkhamGame)
getApiV1ArkhamPendingGameR gameId = do
  void $ fromJustNote "Not authenticated" <$> getRequestUserId
  entity <- runDB $ get404 gameId
  pure $ Entity gameId entity

newtype JoinGameJson = JoinGameJson { deckId :: String }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

putApiV1ArkhamPendingGameR :: ArkhamGameId -> Handler (Entity ArkhamGame)
putApiV1ArkhamPendingGameR gameId = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  let userId' = fromIntegral (fromSqlKey userId)
  JoinGameJson {..} <- requireCheckJsonBody
  ArkhamGame {..} <- runDB $ get404 gameId
  (iid, deck) <- liftIO $ loadDeck deckId
  ge <-
    liftIO
    $ addInvestigator userId' (lookupInvestigator iid) deck
    =<< toInternalGame arkhamGameCurrentData
  runDB $ insert_ $ ArkhamPlayer userId gameId
  Entity gameId (ArkhamGame ge) <$ runDB (replace gameId (ArkhamGame ge))

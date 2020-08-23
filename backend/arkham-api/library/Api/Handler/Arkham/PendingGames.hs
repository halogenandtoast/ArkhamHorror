module Api.Handler.Arkham.PendingGames
  ( getApiV1ArkhamPendingGameR
  , putApiV1ArkhamPendingGameR
  )
where

import Api.Arkham.Helpers
import Arkham.Types.Game
import Arkham.Types.Investigator
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.UUID
import Database.Esqueleto
import Entity.Arkham.GameSetupState
import Entity.Arkham.PendingGame
import Entity.Arkham.Player
import Import hiding (fromString, on, (==.))
import Safe (fromJustNote)

getApiV1ArkhamPendingGameR :: String -> Handler (Entity ArkhamPendingGame)
getApiV1ArkhamPendingGameR tokenStr = do
  void $ fromJustNote "Not authenticated" <$> getRequestUserId
  let token = fromJustNote "Invalid token" $ fromString tokenStr
  runDB $ getBy404 (UniqueArkhamPendingGameToken token)

newtype JoinGameJson = JoinGameJson { deckId :: String }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

putApiV1ArkhamPendingGameR
  :: String -> Handler (Either (Entity ArkhamPendingGame) (Entity ArkhamGame))
putApiV1ArkhamPendingGameR tokenStr = do
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  JoinGameJson {..} <- requireCheckJsonBody
  let token = fromJustNote "Invalid token" $ fromString tokenStr
  Entity pkey ArkhamPendingGame {..} <- runDB
    $ getBy404 (UniqueArkhamPendingGameToken token)
  let
    GameSetupState {..} = arkhamPendingGameGameSetup
    updatedPlayers =
      HashMap.insert (fromIntegral $ fromSqlKey userId) deckId gssPlayers

  if HashMap.size updatedPlayers == gssPlayerCount
    then do
      investigators <-
        for (HashMap.toList updatedPlayers) $ \(userId', deckId') -> do
          (iid, deck) <- liftIO $ loadDeck deckId'
          pure (userId', (lookupInvestigator iid, deck))
      ge <-
        liftIO
        $ runMessages
        =<< newCampaign
              gssCampaignId
              (HashMap.fromList investigators)
              gssDifficulty
      key <- runDB $ do
        gameId <- insert $ ArkhamGame ge
        for_ (HashMap.keys updatedPlayers) $ \userId' ->
          insert_ $ ArkhamPlayer (toSqlKey $ fromIntegral userId') gameId
        pure gameId
      pure $ Right (Entity key (ArkhamGame ge))
    else do
      let
        newPendingGame = ArkhamPendingGame token
          $ arkhamPendingGameGameSetup { gssPlayers = updatedPlayers }
      Left (Entity pkey newPendingGame) <$ runDB (replace pkey newPendingGame)

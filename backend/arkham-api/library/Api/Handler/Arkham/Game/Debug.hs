module Api.Handler.Arkham.Game.Debug (
  getApiV1ArkhamGameExportR,
  postApiV1ArkhamGamesImportR,
  postApiV1ArkhamGamesFixR,
  getApiV1ArkhamGamesReloadR,
) where

import Api.Arkham.Export
import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant
import Arkham.Game
import Conduit
import Data.Text qualified as T
import Data.Time.Clock
import Database.Esqueleto.Experimental hiding (update)
import Database.Persist qualified as Persist
import Entity.Arkham.LogEntry
import Entity.Arkham.Player
import Entity.Arkham.Step
import Import hiding (delete, exists, on, (==.))
import Json
import Safe (fromJustNote)
import UnliftIO.Exception (catch, try)

getApiV1ArkhamGameExportR :: ArkhamGameId -> Handler ArkhamExport
getApiV1ArkhamGameExportR gameId = do
  _ <- fromJustNote "Not authenticated" <$> getRequestUserId
  runDB $ do
    ge <- get404 gameId
    players <- select $ do
      players <- from $ table @ArkhamPlayer
      where_ (players ^. ArkhamPlayerArkhamGameId ==. val gameId)
      pure players
    steps <- select $ do
      steps <- from $ table @ArkhamStep
      where_ $ steps ^. ArkhamStepArkhamGameId ==. val gameId
      orderBy [desc $ steps ^. ArkhamStepStep]
      pure steps

    entries <- getGameLogEntries gameId

    pure
      $ ArkhamExport
        { aeCampaignPlayers = map (arkhamPlayerInvestigatorId . entityVal) players
        , aeCampaignData = arkhamGameToExportData ge (map entityVal steps) entries
        }

postApiV1ArkhamGamesFixR :: Handler ()
postApiV1ArkhamGamesFixR = do
  gameIds <- runDB $ selectKeysList @ArkhamGame [] []
  for_ gameIds $ \gameId -> do
    let handleBrokenGame :: SomeException -> Handler ()
        handleBrokenGame _ = void $ runDB (Persist.delete gameId)
    void (runDB (Persist.get gameId) :: Handler (Maybe ArkhamGame)) `catch` handleBrokenGame

getApiV1ArkhamGamesReloadR :: Handler ()
getApiV1ArkhamGamesReloadR = do
  gameIds <- runDB $ selectKeysList @ArkhamGame [] []
  for_ gameIds $ \gameId -> do
    try @_ @SomeException (runDB $ Persist.get gameId >>= traverse_ (Persist.replace gameId))

  stepIds <- runDB $ selectKeysList @ArkhamStep [] []
  for_ stepIds $ \stepId -> do
    try @_ @SomeException (runDB $ Persist.get stepId >>= traverse_ (Persist.replace stepId))

postApiV1ArkhamGamesImportR :: Handler (PublicGame ArkhamGameId)
postApiV1ArkhamGamesImportR = do
  -- Convert to multiplayer solitaire
  userId <- fromJustNote "Not authenticated" <$> getRequestUserId
  eExportData :: Either String ArkhamExport <-
    fmap eitherDecodeStrict'
      . fileSourceByteString
      . snd
      . fromJustNote "No export file uploaded"
      . headMay
      . snd
      =<< runRequestBody
  now <- liftIO getCurrentTime

  case eExportData of
    Left err -> error $ T.pack err
    Right export -> do
      let
        ArkhamGameExportData {..} = aeCampaignData export
        investigatorIds = aeCampaignPlayers export
      key <- runDB $ do
        gameId <-
          insert
            $ ArkhamGame agedName agedCurrentData agedStep Solo now now
        insertMany_ $ map (\e -> e {arkhamLogEntryArkhamGameId = gameId}) agedLog
        traverse_ (insert_ . ArkhamPlayer userId gameId) investigatorIds
        traverse_
          ( \s ->
              insert_
                $ ArkhamStep gameId (arkhamStepChoice s) (arkhamStepStep s) (arkhamStepActionDiff s)
          )
          agedSteps
        pure gameId
      pure
        $ toPublicGame
          (Entity key $ ArkhamGame agedName agedCurrentData agedStep Solo now now)
          (GameLog $ map arkhamLogEntryBody agedLog)

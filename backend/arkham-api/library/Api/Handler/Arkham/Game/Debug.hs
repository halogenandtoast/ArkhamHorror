module Api.Handler.Arkham.Game.Debug (
  getApiV1ArkhamGamesIdsR,
  getApiV1ArkhamGameExportR,
  postApiV1ArkhamGamesImportR,
  postApiV1ArkhamGamesFixR,
  getApiV1ArkhamGamesReloadR,
  getApiV1ArkhamGameReloadR,
  getApiV1ArkhamGameRawR,
  getApiV1ArkhamGamesRawR,
  postApiV1ArkhamGameRawR,
) where

import Api.Arkham.Export
import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant
import Arkham.Game
import Conduit
import Data.Text qualified as T
import Data.Time.Clock
import Database.Esqueleto.Experimental
import Database.Persist qualified as Persist
import Entity.Arkham.GameRaw
import Entity.Arkham.LogEntry
import Entity.Arkham.Player
import Entity.Arkham.Step
import Import hiding (delete, exists, on, update, (=.), (==.))
import Json
import Safe (fromJustNote)
import UnliftIO.Exception (catch, try)

getApiV1ArkhamGamesIdsR :: Handler [ArkhamGameId]
getApiV1ArkhamGamesIdsR = do
  _ <- fromJustNote "Not authenticated" <$> getRequestUserId
  runDB $ selectKeysList @ArkhamGame [] []

getApiV1ArkhamGameRawR :: ArkhamGameId -> Handler ArkhamGameRaw
getApiV1ArkhamGameRawR gameId = do
  _ <- fromJustNote "Not authenticated" <$> getRequestUserId
  runDB $ get404 (coerce gameId)

getApiV1ArkhamGamesRawR :: Handler [Entity ArkhamGameRaw]
getApiV1ArkhamGamesRawR = do
  _ <- fromJustNote "Not authenticated" <$> getRequestUserId
  runDB $ selectList [] []

postApiV1ArkhamGameRawR :: ArkhamGameId -> Handler ()
postApiV1ArkhamGameRawR gameId = do
  _ <- fromJustNote "Not authenticated" <$> getRequestUserId
  currentData <- requireCheckJsonBody
  runDB $ update \g -> do
    set g [ArkhamGameCurrentData =. val currentData]
    where_ $ g.id ==. val gameId

getApiV1ArkhamGameExportR :: ArkhamGameId -> Handler ArkhamExport
getApiV1ArkhamGameExportR gameId = do
  _ <- fromJustNote "Not authenticated" <$> getRequestUserId
  generateExport gameId

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

getApiV1ArkhamGameReloadR :: ArkhamGameId -> Handler ()
getApiV1ArkhamGameReloadR gameId = do
  _ <- try @_ @SomeException (runDB $ Persist.get gameId >>= traverse_ (Persist.replace gameId))

  stepIds <- runDB $ selectKeysList @ArkhamStep [ArkhamStepArkhamGameId Persist.==. gameId] []
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
        rawExecute "ALTER TABLE arkham_steps DISABLE TRIGGER enforce_step_order_per_game;" []
        for_ agedSteps \s ->
          insert_
            $ ArkhamStep gameId (arkhamStepChoice s) (arkhamStepStep s) (arkhamStepActionDiff s)

        rawExecute "ALTER TABLE arkham_steps ENABLE TRIGGER enforce_step_order_per_game;" []
        pure gameId
      pure
        $ toPublicGame
          (Entity key $ ArkhamGame agedName agedCurrentData agedStep Solo now now)
          (GameLog $ map arkhamLogEntryBody agedLog)

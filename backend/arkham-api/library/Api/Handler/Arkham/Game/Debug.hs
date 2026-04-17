module Api.Handler.Arkham.Game.Debug (
  getApiV1ArkhamGameExportR,
  getApiV1ArkhamGameFullExportR,
  getApiV1ArkhamGameScenarioExportR,
  postApiV1ArkhamGamesImportR,
  postApiV1ArkhamGamesFixR,
  getApiV1ArkhamGamesReloadR,
  getApiV1ArkhamGameReloadR,
  getApiV1ArkhamGameOpenSeatsR,
  postApiV1ArkhamGameClaimSeatR,
) where

import Api.Arkham.Export
import Api.Arkham.Helpers
import Api.Arkham.Types.MultiplayerVariant
import Arkham.Card.CardCode
import Arkham.Game
import Arkham.Id
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

normalizeJsonInvestigatorId :: Text -> Text
normalizeJsonInvestigatorId iid = if "c" `T.isPrefixOf` iid then iid else "c" <> iid

-- Swap the investigator's original player UUID for the new one by doing a
-- text-level replace on the stored JSONB, then casting back.
remapInvestigatorUUID :: ArkhamGameId -> Text -> ArkhamPlayerId -> DB ()
remapInvestigatorUUID gameId iCode newPlayerId = do
  let newUUID = toPathPiece newPlayerId
  results :: [Single (Maybe Text)] <-
    rawSql
      "SELECT current_data->'gameEntities'->'investigators'->?->>'playerId' \
      \FROM arkham_games WHERE id = ?"
      [PersistText iCode, PersistText (toPathPiece gameId)]
  case results of
    (Single (Just origUUID) : _) ->
      rawExecute
        "UPDATE arkham_games \
        \SET current_data = replace(current_data::text, ?, ?)::jsonb \
        \WHERE id = ?"
        [ PersistText ("\"" <> origUUID <> "\"")
        , PersistText ("\"" <> newUUID <> "\"")
        , PersistText (toPathPiece gameId)
        ]
    _ -> pure ()

getApiV1ArkhamGameExportR :: ArkhamGameId -> Handler ArkhamExport
getApiV1ArkhamGameExportR gameId = do
  _ <- getRequestUserId
  generateExport gameId 30

getApiV1ArkhamGameScenarioExportR :: ArkhamGameId -> Handler ArkhamExport
getApiV1ArkhamGameScenarioExportR gameId = do
  _ <- getRequestUserId
  generateScenarioExport gameId

getApiV1ArkhamGameFullExportR :: ArkhamGameId -> Handler TypedContent
getApiV1ArkhamGameFullExportR gameId = do
  (ge, players) <- runDB do
    ge <- get404 gameId
    players <- select do
      p <- from $ table @ArkhamPlayer
      where_ $ p.arkhamGameId ==. val gameId
      pure p
    pure (ge, players)
  let campaignPlayers = map (arkhamPlayerInvestigatorId . entityVal) players
  respondSource "application/json" $ do
    sendChunkLBS "{\"campaignPlayers\":"
    sendChunkLBS $ encode campaignPlayers
    sendChunkLBS ",\"campaignData\":{\"name\":"
    sendChunkLBS $ encode ge.name
    sendChunkLBS ",\"currentData\":"
    sendChunkLBS $ encode ge.currentData
    sendChunkLBS ",\"step\":"
    sendChunkLBS $ encode ge.step
    sendChunkLBS ",\"steps\":["
    sendFlush
    isFirstRef <- liftIO $ newIORef True
    steps <-
      lift $ runDB $ Persist.selectList [ArkhamStepArkhamGameId Persist.==. gameId] [Desc ArkhamStepStep]
    for_ steps $ \(Entity _ s) -> do
      isFirst <- liftIO $ readIORef isFirstRef
      unless isFirst $ sendChunkBS ","
      liftIO $ writeIORef isFirstRef False
      sendChunkLBS $ encode s
      sendFlush
    sendChunkLBS "],\"log\":[],\"multiplayerVariant\":"
    sendChunkLBS $ encode ge.multiplayerVariant
    sendChunkLBS "}}"
    sendFlush

postApiV1ArkhamGamesFixR :: Handler ()
postApiV1ArkhamGamesFixR = do
  gameIds <- runDB $ selectKeysList @ArkhamGame [] []
  for_ gameIds \gameId -> do
    let handleBrokenGame :: SomeException -> Handler ()
        handleBrokenGame _ = void $ runDB (Persist.delete gameId)
    void (runDB (Persist.get gameId) :: Handler (Maybe ArkhamGame)) `catch` handleBrokenGame

getApiV1ArkhamGamesReloadR :: Handler ()
getApiV1ArkhamGamesReloadR = do
  gameIds <- runDB $ selectKeysList @ArkhamGame [] []
  for_ gameIds \gameId -> do
    try @_ @SomeException (runDB $ Persist.get gameId >>= traverse_ (Persist.replace gameId))

  stepIds <- runDB $ selectKeysList @ArkhamStep [] []
  for_ stepIds \stepId -> do
    try @_ @SomeException (runDB $ Persist.get stepId >>= traverse_ (Persist.replace stepId))

getApiV1ArkhamGameReloadR :: ArkhamGameId -> Handler ()
getApiV1ArkhamGameReloadR gameId = do
  _ <- try @_ @SomeException (runDB $ Persist.get gameId >>= traverse_ (Persist.replace gameId))

  stepIds <- runDB $ selectKeysList @ArkhamStep [ArkhamStepArkhamGameId Persist.==. gameId] []
  for_ stepIds \stepId -> do
    try @_ @SomeException (runDB $ Persist.get stepId >>= traverse_ (Persist.replace stepId))

postApiV1ArkhamGamesImportR :: Handler (PublicGame ArkhamGameId)
postApiV1ArkhamGamesImportR = do
  userId <- getRequestUserId
  mVariantOverride <- lookupGetParam "multiplayerVariant"
  (params, files) <- runRequestBody
  let
    mInvestigatorId = fmap normalizeJsonInvestigatorId $ snd <$> find ((== "investigatorId") . fst) params
  eExportData :: Either String ArkhamExport <-
    fmap eitherDecodeStrict'
      . fileSourceByteString
      . snd
      . fromJustNote "No export file uploaded"
      . headMay
      $ files
  now <- liftIO getCurrentTime

  case eExportData of
    Left err -> invalidArgs [T.pack err]
    Right export -> do
      let
        ArkhamGameExportData {..} = aeCampaignData export
        exportVariant = agedMultiplayerVariant
        variant = case mVariantOverride of
          Just "WithFriends" -> WithFriends
          Just "Solo" -> Solo
          _ -> exportVariant
        allInvestigatorIds =
          map normalizeJsonInvestigatorId
            $ maybe [] toList
            $ asum
              [ nonEmpty (map (unCardCode . unInvestigatorId) (gamePlayerOrder agedCurrentData))
              , nonEmpty (aeCampaignPlayers export)
              ]
        campaignInvestigatorIds = map normalizeJsonInvestigatorId $ aeCampaignPlayers export
      key <- runDB $ do
        gameId <- insert $ ArkhamGame agedName agedCurrentData agedStep variant now now
        case variant of
          Solo -> do
            iid <- case headMay allInvestigatorIds of
              Nothing -> lift $ invalidArgs ["No investigators found in game data"]
              Just iid -> pure iid
            insert_ $ ArkhamPlayer userId gameId iid
          WithFriends -> do
            let mChosen = mInvestigatorId <|> headMay campaignInvestigatorIds
            chosenInvestigator <- case mChosen of
              Nothing -> lift $ invalidArgs ["No investigator specified"]
              Just iid -> pure iid
            newPlayerId <- insert $ ArkhamPlayer userId gameId chosenInvestigator
            remapInvestigatorUUID gameId chosenInvestigator newPlayerId
        rawExecute
          "DO $$ \
          \BEGIN \
          \  IF EXISTS ( \
          \    SELECT 1 \
          \    FROM pg_trigger t \
          \    JOIN pg_class c ON c.oid = t.tgrelid \
          \    JOIN pg_namespace n ON n.oid = c.relnamespace \
          \    WHERE t.tgname = 'enforce_step_order_per_game' \
          \      AND c.relname = 'arkham_steps' \
          \      AND n.nspname = 'public' \
          \  ) THEN \
          \    EXECUTE 'ALTER TABLE public.arkham_steps DISABLE TRIGGER enforce_step_order_per_game'; \
          \  END IF; \
          \END$$;"
          []
        insertMany_ [ArkhamStep gameId s.choice s.step s.actionDiff | s <- agedSteps]

        rawExecute
          "DO $$ \
          \BEGIN \
          \  IF EXISTS ( \
          \    SELECT 1 \
          \    FROM pg_trigger t \
          \    JOIN pg_class c ON c.oid = t.tgrelid \
          \    JOIN pg_namespace n ON n.oid = c.relnamespace \
          \    WHERE t.tgname = 'enforce_step_order_per_game' \
          \      AND c.relname = 'arkham_steps' \
          \      AND n.nspname = 'public' \
          \  ) THEN \
          \    EXECUTE 'ALTER TABLE public.arkham_steps ENABLE TRIGGER enforce_step_order_per_game'; \
          \  END IF; \
          \END$$;"
          []
        pure gameId
      pure
        $ toPublicGame
          (Entity key $ ArkhamGame agedName agedCurrentData agedStep variant now now)
          (GameLog $ map arkhamLogEntryBody agedLog)

getApiV1ArkhamGameOpenSeatsR :: ArkhamGameId -> Handler [Text]
getApiV1ArkhamGameOpenSeatsR gameId = do
  _ <- getRequestUserId
  runDB do
    g <- get404 gameId
    let allInvestigators =
          map (normalizeJsonInvestigatorId . unCardCode . unInvestigatorId) $ gamePlayerOrder g.currentData
    assignedInvestigators <-
      map unValue <$> select do
        players <- from $ table @ArkhamPlayer
        where_ $ players.arkhamGameId ==. val gameId
        pure players.investigatorId
    pure $ filter (`notElem` assignedInvestigators) allInvestigators

data ClaimSeatPost = ClaimSeatPost
  { investigatorId :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

postApiV1ArkhamGameClaimSeatR :: ArkhamGameId -> Handler ()
postApiV1ArkhamGameClaimSeatR gameId = do
  userId <- getRequestUserId
  ClaimSeatPost {investigatorId = rawId} <- requireCheckJsonBody
  let investigatorId = normalizeJsonInvestigatorId rawId
  runDB do
    g <- get404 gameId
    when (g.multiplayerVariant /= WithFriends) do
      lift $ permissionDenied "This game is not a multiplayer game"
    let allInvestigators =
          map (normalizeJsonInvestigatorId . unCardCode . unInvestigatorId)
            $ gamePlayerOrder g.currentData
    unless (investigatorId `elem` allInvestigators) do
      lift $ invalidArgs ["Invalid investigator for this game"]
    mTaken <- selectOne do
      players <- from $ table @ArkhamPlayer
      where_ $ players.arkhamGameId ==. val gameId
      where_ $ players.investigatorId ==. val investigatorId
      pure players
    when (isJust mTaken) do
      lift $ permissionDenied "This seat is already taken"
    mAlreadyJoined <- getBy $ UniquePlayer userId gameId
    when (isJust mAlreadyJoined) do
      lift $ permissionDenied "You already have a seat in this game"
    newPlayerId <- insert $ ArkhamPlayer userId gameId investigatorId
    remapInvestigatorUUID gameId investigatorId newPlayerId

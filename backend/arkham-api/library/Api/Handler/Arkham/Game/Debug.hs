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
import Database.Persist.Sql (Single (..), rawSql)
import Entity.Arkham.LogEntry
import Entity.Arkham.Player
import Entity.Arkham.Step
import Import hiding (delete, exists, on, (==.))
import Json
import Safe (fromJustNote)
import UnliftIO.Exception (catch, try)

-- | Ensure investigator ID has the 'c' prefix (e.g. "03004" -> "c03004")
addC :: Text -> Text
addC iid = if "c" `T.isPrefixOf` iid then iid else "c" <> iid

-- | Replace the original player UUID with the new local ArkhamPlayer UUID.
-- Uses jsonb_set to target the exact path, avoiding accidental replacements.
remapInvestigatorUUID
  :: ArkhamGameId
  -> Text           -- ^ investigator card code, e.g. "c03004"
  -> ArkhamPlayerId
  -> SqlPersistT Handler ()
remapInvestigatorUUID gameId iCode newPlayerId = do
  let newUUID = toPathPiece newPlayerId
      normalizedCode = addC iCode
  results :: [Single (Maybe Text)] <- rawSql
    "SELECT current_data->'gameEntities'->'investigators'->?->>'playerId' \
    \FROM arkham_games WHERE id = ?"
    [PersistText normalizedCode, PersistText (toPathPiece gameId)]
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

getApiV1ArkhamGameFullExportR :: ArkhamGameId -> Handler ArkhamExport
getApiV1ArkhamGameFullExportR gameId = generateFullExport gameId

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
  userId <- getRequestUserId
  (params, files) <- runRequestBody
  let mInvestigatorId = fmap addC $ snd <$> find ((== "investigatorId") . fst) params
  let mVariantOverride = snd <$> find ((== "multiplayerVariant") . fst) params
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
        investigatorIds = map addC $ aeCampaignPlayers export
        exportVariant = agedMultiplayerVariant
        variant = case mVariantOverride of
          Just "WithFriends" -> WithFriends
          Just "Solo"        -> Solo
          _                  -> exportVariant
      key <- runDB $ do
        gameId <- insert $ ArkhamGame agedName agedCurrentData agedStep variant now now
        case variant of
          Solo ->
            traverse_ (insert_ . ArkhamPlayer userId gameId) investigatorIds
          WithFriends -> do
            let mChosen = mInvestigatorId <|> headMay investigatorIds
            chosenInvestigator <- case mChosen of
              Nothing  -> lift $ invalidArgs ["No investigator specified"]
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
          \END$$;" []
        for_ agedSteps \s ->
          insert_
            $ ArkhamStep gameId (arkhamStepChoice s) (arkhamStepStep s) (arkhamStepActionDiff s)
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
          \END$$;" []
        pure gameId
      pure
        $ toPublicGame
          (Entity key $ ArkhamGame agedName agedCurrentData agedStep variant now now)
          (GameLog $ map arkhamLogEntryBody agedLog)

-- | Returns investigator IDs with no player assigned yet.
getApiV1ArkhamGameOpenSeatsR :: ArkhamGameId -> Handler [Text]
getApiV1ArkhamGameOpenSeatsR gameId = do
  _ <- getRequestUserId
  runDB do
    g <- get404 gameId
    let allInvestigators =
          map (addC . unCardCode . unInvestigatorId)
            . gamePlayerOrder
            $ arkhamGameCurrentData g
    assignedInvestigators <-
      fmap (map (arkhamPlayerInvestigatorId . entityVal))
        . select
        $ do
          players <- from $ table @ArkhamPlayer
          where_ (players ^. ArkhamPlayerArkhamGameId ==. val gameId)
          pure players
    pure $ filter (`notElem` assignedInvestigators) allInvestigators

data ClaimSeatPost = ClaimSeatPost
  { investigatorId :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

-- | Claim an open seat in a WithFriends game.
postApiV1ArkhamGameClaimSeatR :: ArkhamGameId -> Handler ()
postApiV1ArkhamGameClaimSeatR gameId = do
  userId <- getRequestUserId
  ClaimSeatPost {investigatorId = rawId} <- requireCheckJsonBody
  let investigatorId = addC rawId
  runDB do
    g <- get404 gameId
    when (arkhamGameMultiplayerVariant g /= WithFriends)
      $ lift $ permissionDenied "This game is not a multiplayer game"
    let allInvestigators =
          map (addC . unCardCode . unInvestigatorId)
            . gamePlayerOrder
            $ arkhamGameCurrentData g
    unless (investigatorId `elem` allInvestigators)
      $ lift $ invalidArgs ["Invalid investigator for this game"]
    mTaken <- selectOne $ do
      players <- from $ table @ArkhamPlayer
      where_
        $ players ^. ArkhamPlayerArkhamGameId ==. val gameId
        &&. players ^. ArkhamPlayerInvestigatorId ==. val investigatorId
      pure players
    when (isJust mTaken)
      $ lift $ permissionDenied "This seat is already taken"
    mAlreadyJoined <- getBy (UniquePlayer userId gameId)
    when (isJust mAlreadyJoined)
      $ lift $ permissionDenied "You already have a seat in this game"
    newPlayerId <- insert $ ArkhamPlayer userId gameId investigatorId
    remapInvestigatorUUID gameId investigatorId newPlayerId

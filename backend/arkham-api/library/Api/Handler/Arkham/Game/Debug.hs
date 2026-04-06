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

-- | Replace the original player UUID (from an exported game) with the new
-- local ArkhamPlayer UUID in the stored game JSONB.
-- Reads the original UUID via SQL from the investigators path in current_data.
remapInvestigatorUUID
  :: ArkhamGameId
  -> Text           -- ^ investigator card code, e.g. "c03004"
  -> ArkhamPlayerId
  -> SqlPersistT Handler ()
remapInvestigatorUUID gameId iCode newPlayerId = do
  let newUUID = toPathPiece newPlayerId
      normalizedCode = addC iCode
  results <- rawSql
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
  -- Convert to multiplayer solitaire
  userId <- getRequestUserId
  (params, files) <- runRequestBody
  let mInvestigatorId = snd <$> find ((== "investigatorId") . fst) params
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
    Left err -> error $ T.pack err
    Right export -> do
      let
        ArkhamGameExportData {..} = aeCampaignData export
        investigatorIds = aeCampaignPlayers export
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
            let chosenInvestigator = fromMaybe (fromMaybe "00000" (headMay investigatorIds)) mInvestigatorId
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

-- | Returns the list of investigator IDs that have no player assigned yet.
-- Only relevant for WithFriends games imported via postApiV1ArkhamGamesImportR.
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

-- | Claim an open seat in a WithFriends game by choosing an investigator.
postApiV1ArkhamGameClaimSeatR :: ArkhamGameId -> Handler ()
postApiV1ArkhamGameClaimSeatR gameId = do
  userId <- getRequestUserId
  ClaimSeatPost {investigatorId} <- requireCheckJsonBody
  runDB do
    g <- get404 gameId
    -- Only valid for multiplayer games
    when (arkhamGameMultiplayerVariant g /= WithFriends)
      $ error "This game is not a multiplayer game"
    -- Validate investigator is in the game
    let allInvestigators =
          map (addC . unCardCode . unInvestigatorId)
            . gamePlayerOrder
            $ arkhamGameCurrentData g
    unless (investigatorId `elem` allInvestigators)
      $ error "Invalid investigator for this game"
    -- Check the seat is not already taken
    mTaken <- selectOne $ do
      players <- from $ table @ArkhamPlayer
      where_
        $ players ^. ArkhamPlayerArkhamGameId ==. val gameId
        &&. players ^. ArkhamPlayerInvestigatorId ==. val investigatorId
      pure players
    when (isJust mTaken)
      $ error "This seat is already taken"
    -- Check user doesn't already have a seat in this game
    mAlreadyJoined <- getBy (UniquePlayer userId gameId)
    when (isJust mAlreadyJoined)
      $ error "You already have a seat in this game"
    newPlayerId <- insert $ ArkhamPlayer userId gameId investigatorId
    remapInvestigatorUUID gameId investigatorId newPlayerId

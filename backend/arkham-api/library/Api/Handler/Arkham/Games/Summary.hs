{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | The game lists, without deserializing the games.

'toGameDetailsEntry' answers the same question by parsing @current_data@ into a
whole 'Arkham.Game.Game' -- every location, asset, enemy, treachery and card --
and then reading nine small facts off the result. The column averages 97kB of
JSON and runs to several hundred for a late campaign, the home page's list is
unbounded, and every row pays the parse: a player with a hundred games waits for
a hundred games to be built so the page can print a hundred titles. Worse,
'FromJSON' 'Arkham.Game.Game' registers the game's custom cards as it goes, and
that drops every compiled card for every game on the process, so merely drawing
the list disturbs the games being played.

So ask Postgres for the nine facts instead. The paths below restate the
serialized shape of 'Arkham.Game.Game', 'CampaignAttrs' and 'ScenarioAttrs' --
@gameMode@ is a 'Data.These.These', whose encoding files the campaign under
@This@ and the scenario under @That@ -- but only the paths: what comes back is
decoded by those same types' 'FromJSON' instances, so a renamed constructor is
still a decode failure rather than a wrong answer on the page.

'summaryEntry' has to agree with 'toGameDetailsEntry', which still serves the
admin reads. That duplication is the price, and the reason it is worth paying is
the two orders of magnitude: 614 bytes a row against 97kB, and no game built at
all.
-}
module Api.Handler.Arkham.Games.Summary (
  ownGameSummaries,
  spectatableGameSummaries,
  spectatableGamesLimit,
) where

import Api.Arkham.Types.MultiplayerVariant
import Api.Handler.Arkham.Games.Shared (
  CampaignDetails (..),
  GameDetails (..),
  GameDetailsEntry (..),
  InvestigatorDetails (..),
  ScenarioDetails (..),
 )
import Arkham.CampaignStep (CampaignStep)
import Arkham.Campaigns.TheDreamEaters.Meta qualified as TheDreamEaters
import Arkham.Card.CardCode (CardCode)
import Arkham.Card.CustomCard (CustomCard, registerCustomCards)
import Arkham.ClassSymbol (ClassSymbol)
import Arkham.Difficulty (Difficulty)
import Arkham.Game.Outcome (
  Ending,
  GameOutcome,
  inferCampaignOutcome,
  outcomeEnding,
  standaloneOutcome,
 )
import Arkham.Game.State (GameState (IsOver))
import Arkham.Id (CampaignId, InvestigatorId, PlayerId (..), ScenarioId)
import Arkham.Investigator (lookupInvestigator)
import Arkham.Name (Name)
import Arkham.Resolution (Resolution)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.UUID (nil)
import Database.Persist.Postgresql.JSON ()
import Database.Persist.Sql (Single (..), rawSql)
import Import
import Json hiding (Success)
import UnliftIO.Exception (evaluate, try)

-- ------------------------------------------------------------------- reads ---

-- | The games this user has a seat in, newest activity first.
ownGameSummaries :: UserId -> DB [GameDetailsEntry]
ownGameSummaries userId =
  querySummaries
    "WHERE EXISTS (SELECT 1 FROM arkham_players p WHERE p.arkham_game_id = g.id AND p.user_id = ?) \
    \  AND NOT EXISTS (SELECT 1 FROM arkham_epic_groups eg WHERE eg.arkham_game_id = g.id) \
    \ORDER BY g.updated_at DESC"
    [toPersistValue userId]

{- | How many games the watch list will carry.

A row no longer costs a game, but the list is still a page of things to pick
from rather than an inventory, and newest activity first is the order somebody
choosing a game to watch wants.
-}
spectatableGamesLimit :: Int64
spectatableGamesLimit = 50

{- | The games whose players have opened them up to be watched.

Mirrors @spectatableGames@, which the digest still uses: advertised, in play,
not one this user has a seat in, and not an Epic group.
-}
spectatableGameSummaries :: UserId -> DB [GameDetailsEntry]
spectatableGameSummaries userId =
  querySummaries
    ( "WHERE g.allow_spectators \
      \  AND keys.top #>> '{gameGameState,tag}' = 'IsActive' \
      \  AND NOT EXISTS (SELECT 1 FROM arkham_players p WHERE p.arkham_game_id = g.id AND p.user_id = ?) \
      \  AND NOT EXISTS (SELECT 1 FROM arkham_epic_groups eg WHERE eg.arkham_game_id = g.id) \
      \ORDER BY g.updated_at DESC \
      \LIMIT "
        <> tshow spectatableGamesLimit
    )
    [toPersistValue userId]

{- | The projection: one compact jsonb object per game, averaging 614 bytes
against the 97kB the row holds.

The keys are 'GameSummary''s field names, so a mismatch fails at decode rather
than quietly reporting the wrong game. A missing path is SQL @NULL@, which
'jsonb_build_object' writes as JSON @null@ and aeson reads back as 'Nothing';
the two aggregates are coalesced instead, because an absent object yields no
rows to aggregate and @NULL@ is not an empty list.

Everything reads through the @keys@ lateral rather than touching
@current_data@ again. Postgres does not reuse a detoasted value between
expressions, so each mention of the column is another 97kB fetched and parsed:
the twenty mentions this projection needs cost 1.3ms a row, and taking the six
top-level keys once costs 0.5ms.

The lateral aggregates without grouping, so it yields exactly one row -- @top@
being @NULL@ for a game carrying none of those keys -- and the @CROSS JOIN@
cannot drop a game.
-}
summarySql :: Text
summarySql =
  "SELECT g.id, jsonb_build_object( \
  \  'name', g.name, \
  \  'multiplayerVariant', g.multiplayer_variant, \
  \  'playerCount', (SELECT count(*) FROM arkham_players pc WHERE pc.arkham_game_id = g.id), \
  \  'gameState', keys.top -> 'gameGameState', \
  \  'outcome', keys.top -> 'gameOutcome', \
  \  'lastResolution', keys.top -> 'gameLastResolution', \
  \  'customCards', COALESCE(keys.top -> 'gameCustomCards', '{}'::jsonb), \
  \  'investigators', COALESCE((SELECT jsonb_object_agg(k, v -> 'class') \
  \                             FROM jsonb_each(keys.top #> '{gameEntities,investigators}') AS e(k, v)), '{}'::jsonb), \
  \  'campaign', CASE WHEN keys.top #> '{gameMode,This}' IS NULL THEN NULL ELSE jsonb_build_object( \
  \     'id', keys.top #> '{gameMode,This,id}', \
  \     'difficulty', keys.top #> '{gameMode,This,difficulty}', \
  \     'step', keys.top #> '{gameMode,This,step}', \
  \     'completedSteps', keys.top #> '{gameMode,This,completedSteps}', \
  \     'resolutions', keys.top #> '{gameMode,This,resolutions}', \
  \     'currentCampaignMode', keys.top #> '{gameMode,This,meta,currentCampaignMode}', \
  \     'otherInvestigators', COALESCE((SELECT jsonb_agg(k) \
  \                                     FROM jsonb_object_keys(keys.top #> '{gameMode,This,meta,otherCampaignAttrs,decks}') AS k), '[]'::jsonb) \
  \  ) END, \
  \  'scenario', CASE WHEN keys.top #> '{gameMode,That}' IS NULL THEN NULL ELSE jsonb_build_object( \
  \     'id', keys.top #> '{gameMode,That,id}', \
  \     'difficulty', keys.top #> '{gameMode,That,difficulty}', \
  \     'name', keys.top #> '{gameMode,That,name}', \
  \     'variant', keys.top #> '{gameMode,That,meta,variant}' \
  \  ) END \
  \) \
  \FROM arkham_games g \
  \CROSS JOIN LATERAL ( \
  \  SELECT jsonb_object_agg(k, v) AS top \
  \  FROM jsonb_each(g.current_data) AS t(k, v) \
  \  WHERE k IN ('gameMode', 'gameEntities', 'gameGameState', 'gameOutcome', 'gameLastResolution', 'gameCustomCards') \
  \) AS keys "

{- | Run the projection and turn each row into the entry the list returns.

The id travels as its own column rather than inside the object: it is the one
field with no counterpart in the game's JSON, and 'PersistField' already knows
how to read a key.
-}
querySummaries :: Text -> [PersistValue] -> DB [GameDetailsEntry]
querySummaries predicate params = do
  rows :: [(Single ArkhamGameId, Single Value)] <- rawSql (summarySql <> predicate) params
  forM rows \(Single gameId, Single row) -> summaryEntry gameId row

-- -------------------------------------------------------------- the shapes ---

data ScenarioSummary = ScenarioSummary
  { id :: ScenarioId
  , difficulty :: Difficulty
  , name :: Name
  , variant :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

data CampaignSummary = CampaignSummary
  { id :: CampaignId
  , difficulty :: Difficulty
  , step :: CampaignStep
  , completedSteps :: [CampaignStep]
  , resolutions :: Map ScenarioId Resolution
  , currentCampaignMode :: Maybe TheDreamEaters.CampaignPart
  , otherInvestigators :: [InvestigatorId]
  {- ^ Investigators the campaign has set aside, by id. Taken as the keys of
  the other campaign's decks, which is where 'toGameDetailsEntry' reads them
  from; only The Dream-Eaters has any.
  -}
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

data GameSummary = GameSummary
  { name :: Text
  , multiplayerVariant :: MultiplayerVariant
  , playerCount :: Int
  , gameState :: GameState
  , outcome :: Maybe GameOutcome
  , lastResolution :: Maybe Ending
  , customCards :: Map CardCode CustomCard
  , investigators :: Map InvestigatorId ClassSymbol
  , campaign :: Maybe CampaignSummary
  , scenario :: Maybe ScenarioSummary
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

-- --------------------------------------------------------------- the entry ---

{- | A row as the list reports it.

A decode failure is reported as the same failed entry 'tryGameDetailsEntry'
produces, so one unreadable game still leaves the rest of the list renderable
and the broken game deletable. The name comes from the row's own column rather
than from the game's JSON, so a game can still be named in the failure.
-}
summaryEntry :: MonadIO m => ArkhamGameId -> Value -> m GameDetailsEntry
summaryEntry gameId row = case parseEither parseJSON row of
  Left err -> pure $ FailedGameDetails gameId (rowName row) (T.pack err)
  Right summary -> do
    {- 'lookupInvestigator' resolves a custom investigator through the card
    registry, so this game's own definitions have to be in it first. Paid only
    for the rare campaign that sets anybody aside, because registering drops
    every compiled card for every game on the process. -}
    let otherIds = maybe [] (.otherInvestigators) summary.campaign
    unless (null otherIds || Map.null summary.customCards)
      $ liftIO
      $ registerCustomCards summary.customCards
    -- 'lookupInvestigator' throws on a card code it cannot resolve, so force
    -- the encoding here rather than part-way through the handler's response.
    result <- liftIO $ try @_ @SomeException $ do
      let details = summaryDetails gameId summary
      void $ evaluate $ BSL.length $ encode details
      pure details
    pure $ case result of
      Right details -> SuccessGameDetails details
      Left err -> FailedGameDetails gameId summary.name (tshow err)
 where
  -- A row that will not decode may still carry a readable name.
  rowName = \case
    Object o -> case KeyMap.lookup "name" o of
      Just (String t) -> t
      _ -> ""
    _ -> ""

summaryDetails :: ArkhamGameId -> GameSummary -> GameDetails
summaryDetails gameId s =
  GameDetails
    { id = gameId
    , scenario =
        s.scenario <&> \sc -> ScenarioDetails sc.id sc.difficulty sc.name sc.variant
    , campaign =
        s.campaign <&> \c -> CampaignDetails c.id c.difficulty c.currentCampaignMode
    , gameState = s.gameState
    , outcome = summaryOutcome s
    , name = s.name
    , investigators = investigators
    , otherInvestigators = otherInvestigators
    , multiplayerVariant = s.multiplayerVariant
    , hasOpenSeats = s.multiplayerVariant == WithFriends && s.playerCount < length investigators
    }
 where
  investigators = [InvestigatorDetails iid cls | (iid, cls) <- Map.toList s.investigators]
  otherInvestigators =
    [ InvestigatorDetails iid (lookupInvestigator iid (PlayerId nil)).classSymbol
    | iid <- maybe [] (.otherInvestigators) s.campaign
    ]

{- | What 'Arkham.Game.Utils.gameOutcomeOf' says, from the same four facts.

A recorded outcome always wins; failing that one is reconstructed, which is all
a game that finished before outcomes were recorded has left behind.
-}
summaryOutcome :: GameSummary -> Maybe GameOutcome
summaryOutcome s
  | s.gameState /= IsOver = Nothing
  | otherwise = s.outcome <|> inferred
 where
  withEnding o = o {outcomeEnding = s.lastResolution <|> outcomeEnding o}
  inferred = case s.campaign of
    Just c -> withEnding <$> inferCampaignOutcome c.id c.step c.completedSteps c.resolutions
    Nothing -> standaloneOutcome <$> s.lastResolution

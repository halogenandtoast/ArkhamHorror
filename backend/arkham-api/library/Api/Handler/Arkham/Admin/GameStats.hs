{- | Aggregate play statistics for the admin panel.

Every fact worth counting about a game lives inside @arkham_games.current_data@, a
jsonb blob averaging 28kB. Extracting one field across the table costs about
0.7ms per game -- the blob has to be detoasted and parsed each time -- so doing it
per request, a dozen times over, is not a page anyone would wait for.

So it is done once, into the @arkham_game_stats@ materialized view (see the
@arkham_game_stats@ migration), and every query here reads narrow indexed columns
off that. The view ships unpopulated, because building it costs time proportional
to a table whose size on any given deployment is unknown; 'populated' says whether
it has been built, and the panel offers to do it.

Achievements are left out of the view: that table is already relational and
already indexed, so those counts are read live and are always current.

Scenario and campaign codes come back as the engine spells them, and 'names'
carries a code-to-title dictionary built from the engine's own registries, so the
client renders real titles without loading the 2MB card catalogue.
-}
module Api.Handler.Arkham.Admin.GameStats (
  getApiV1AdminGameStatsR,
  postApiV1AdminGameStatsRefreshR,
) where

import Arkham.Campaign (allCampaigns, lookupCampaign)
import Arkham.Campaign.Types (campaignName)
import Arkham.Card.CardCode (CardCode (..))
import Arkham.Card.CardDef (cdName)
import Arkham.Classes.Entity (toAttrs)
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Id (CampaignId (..), ScenarioId (..))
import Arkham.Name (display)
import Arkham.Scenario (allScenarioCards)
import Arkham.SideStory (sideStoryIds)
import Data.Aeson.Types (parseEither)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock
import Database.Persist.Postgresql.JSON ()
import Database.Persist.Sql (Single (..), rawExecute, rawSql)
import Import hiding ((==.))
import Import qualified as P
import Json hiding (Success)
import UnliftIO.Exception (throwString)

-- ----------------------------------------------------------------- responses ---

data Totals = Totals
  { totalsGames :: Int
  , totalsCampaignGames :: Int
  , totalsStandaloneGames :: Int
  , totalsFinished :: Int
  , totalsInProgress :: Int
  , totalsNeverStarted :: Int
  {- ^ Never got past deck selection. Worth separating from "in progress":
  these are games nobody actually sat down to.
  -}
  , totalsPlayers :: Int
  }
  deriving stock Generic

instance ToJSON Totals where
  toJSON = genericToJSON $ aesonOptions $ Just "totals"
  toEncoding = genericToEncoding $ aesonOptions $ Just "totals"

instance FromJSON Totals where
  parseJSON = genericParseJSON $ aesonOptions $ Just "totals"

{- | One campaign's record. @finished@ is a game that reached an ending, win or
lose; @scenariosPlayed@/@scenariosWon@ are summed across every game of it, which
is what makes a success rate meaningful for a campaign nobody has finished yet.
-}
data CampaignStat = CampaignStat
  { campaignStatId :: Text
  , campaignStatGames :: Int
  , campaignStatFinished :: Int
  , campaignStatInProgress :: Int
  , campaignStatNeverStarted :: Int
  , campaignStatScenariosPlayed :: Int
  , campaignStatScenariosWon :: Int
  , campaignStatPlayers1 :: Int
  , campaignStatPlayers2 :: Int
  , campaignStatPlayers3 :: Int
  , campaignStatPlayers4 :: Int
  , campaignStatEasy :: Int
  , campaignStatStandard :: Int
  , campaignStatHard :: Int
  , campaignStatExpert :: Int
  }
  deriving stock Generic

instance ToJSON CampaignStat where
  toJSON = genericToJSON $ aesonOptions $ Just "campaignStat"
  toEncoding = genericToEncoding $ aesonOptions $ Just "campaignStat"

instance FromJSON CampaignStat where
  parseJSON = genericParseJSON $ aesonOptions $ Just "campaignStat"

-- | A scenario played on its own rather than inside a campaign.
data StandaloneStat = StandaloneStat
  { standaloneStatId :: Text
  , standaloneStatGames :: Int
  , standaloneStatFinished :: Int
  , standaloneStatIsSideStory :: Bool
  }
  deriving stock Generic

instance ToJSON StandaloneStat where
  toJSON = genericToJSON $ aesonOptions $ Just "standaloneStat"
  toEncoding = genericToEncoding $ aesonOptions $ Just "standaloneStat"

instance FromJSON StandaloneStat where
  parseJSON = genericParseJSON $ aesonOptions $ Just "standaloneStat"

data CountStat = CountStat {countStatKey :: Text, countStatCount :: Int}
  deriving stock Generic

instance ToJSON CountStat where
  toJSON = genericToJSON $ aesonOptions $ Just "countStat"
  toEncoding = genericToEncoding $ aesonOptions $ Just "countStat"

instance FromJSON CountStat where
  parseJSON = genericParseJSON $ aesonOptions $ Just "countStat"

-- | A side story taken during a campaign, and how often that pairing happens.
data SideStoryInCampaign = SideStoryInCampaign
  { sideStoryInCampaignCampaignId :: Text
  , sideStoryInCampaignScenarioId :: Text
  , sideStoryInCampaignCount :: Int
  }
  deriving stock Generic

instance ToJSON SideStoryInCampaign where
  toJSON = genericToJSON $ aesonOptions $ Just "sideStoryInCampaign"
  toEncoding = genericToEncoding $ aesonOptions $ Just "sideStoryInCampaign"

instance FromJSON SideStoryInCampaign where
  parseJSON = genericParseJSON $ aesonOptions $ Just "sideStoryInCampaign"

{- | How a scenario tends to go: how many campaign runs finished it, and how many
of those came out with a resolution rather than resigning or being wiped out.
-}
data ScenarioOutcome = ScenarioOutcome
  { scenarioOutcomeId :: Text
  , scenarioOutcomePlayed :: Int
  , scenarioOutcomeWon :: Int
  }
  deriving stock Generic

instance ToJSON ScenarioOutcome where
  toJSON = genericToJSON $ aesonOptions $ Just "scenarioOutcome"
  toEncoding = genericToEncoding $ aesonOptions $ Just "scenarioOutcome"

instance FromJSON ScenarioOutcome where
  parseJSON = genericParseJSON $ aesonOptions $ Just "scenarioOutcome"

{- | Where campaign runs get to before they stop: how many have completed exactly
N scenarios. The drop-off from one N to the next is where people are giving up.
-}
data CampaignProgress = CampaignProgress
  { campaignProgressCampaignId :: Text
  , campaignProgressScenarios :: Int
  , campaignProgressGames :: Int
  }
  deriving stock Generic

instance ToJSON CampaignProgress where
  toJSON = genericToJSON $ aesonOptions $ Just "campaignProgress"
  toEncoding = genericToEncoding $ aesonOptions $ Just "campaignProgress"

instance FromJSON CampaignProgress where
  parseJSON = genericParseJSON $ aesonOptions $ Just "campaignProgress"

data AchievementStat = AchievementStat
  { achievementStatId :: Text
  , achievementStatEarned :: Int
  , achievementStatInProgress :: Int
  {- ^ Has a progress row but has not earned it. Against @earned@ this says
  whether an achievement is hard or merely never noticed.
  -}
  }
  deriving stock Generic

instance ToJSON AchievementStat where
  toJSON = genericToJSON $ aesonOptions $ Just "achievementStat"
  toEncoding = genericToEncoding $ aesonOptions $ Just "achievementStat"

instance FromJSON AchievementStat where
  parseJSON = genericParseJSON $ aesonOptions $ Just "achievementStat"

data GameStatsResponse = GameStatsResponse
  { gameStatsResponsePopulated :: Bool
  , gameStatsResponseRefreshedAt :: Maybe UTCTime
  , gameStatsResponseRefreshDurationMs :: Maybe Int
  , gameStatsResponseTotals :: Totals
  , gameStatsResponseCampaigns :: [CampaignStat]
  , gameStatsResponseStandalones :: [StandaloneStat]
  , gameStatsResponsePlayerCounts :: [CountStat]
  , gameStatsResponseDifficulties :: [CountStat]
  , gameStatsResponseVariants :: [CountStat]
  , gameStatsResponseSideStoriesInCampaigns :: [SideStoryInCampaign]
  , gameStatsResponseScenarioOutcomes :: [ScenarioOutcome]
  , gameStatsResponseCampaignProgress :: [CampaignProgress]
  , gameStatsResponseAchievements :: [AchievementStat]
  , gameStatsResponseAchievementUsers :: Int
  {- ^ Users who have earned at least one achievement: the honest denominator
  for "what share of people have this one", since a user who has never
  played is not someone who failed to earn it.
  -}
  , gameStatsResponseMonthly :: [CountStat]
  , gameStatsResponseNames :: Map Text Text
  -- ^ Code to title, for every campaign and scenario the engine knows.
  }
  deriving stock Generic

instance ToJSON GameStatsResponse where
  toJSON = genericToJSON $ aesonOptions $ Just "gameStatsResponse"
  toEncoding = genericToEncoding $ aesonOptions $ Just "gameStatsResponse"

-- --------------------------------------------------------------------- names ---

{- | Titles for every campaign and scenario, built once per process.

A top-level binding rather than a function: these come out of the engine's
registries, which do not change while the server runs, and constructing every
campaign to read its name is not worth repeating per request.
-}
codeNames :: Map Text Text
codeNames = Map.fromList (campaignEntries <> scenarioEntries)
 where
  campaignEntries =
    [ (unCampaignId cid, campaignName (toAttrs (lookupCampaign cid Easy)))
    | cid <- Map.keys allCampaigns
    ]
  scenarioEntries =
    [ (unCardCode code, display (cdName def))
    | (code, def) <- Map.toList allScenarioCards
    ]

-- --------------------------------------------------------------------- reads ---

{- | Run a statistics query and decode its rows through the record's JSON instance.

Postgres aggregates the result set into one jsonb value rather than handing back
columns. Two reasons: 'rawSql' has a tuple-arity ceiling that the fifteen-column
campaign row is well past, and matching a long positional tuple to a record is
exactly the shape of mistake that silently transposes two @Int@s. Here the column
aliases in the SQL *are* the record's JSON field names, so a mismatch fails loudly
at decode rather than quietly reporting the wrong number.

The aliases must be double-quoted in the SQL wherever they are not all lowercase,
since Postgres folds unquoted identifiers.
-}
queryJson :: FromJSON a => Text -> [PersistValue] -> DB [a]
queryJson sql params = do
  rows :: [Single Value] <-
    rawSql
      ("SELECT COALESCE(jsonb_agg(t), '[]'::jsonb) FROM (" <> sql <> ") AS t")
      params
  case rows of
    [] -> pure []
    Single value : _ -> case parseEither parseJSON value of
      Right as -> pure as
      Left err -> throwString $ "game stats: could not decode rows: " <> err

{- | Whether the view has been built.

Read out of the catalogue rather than by querying the view and handling the
failure: selecting from an unpopulated materialized view is an error, and a panel
asking "is there anything here yet" should not have to provoke one.
-}
viewPopulated :: DB Bool
viewPopulated = do
  rows :: [Single Bool] <-
    rawSql
      "SELECT ispopulated FROM pg_matviews WHERE matviewname = 'arkham_game_stats'"
      []
  pure $ case rows of
    Single populated : _ -> populated
    [] -> False

{- | The side-story list, as one comma-separated parameter.

Passed as text and split in SQL rather than bound as an array: it is a fixed list
the engine owns, and this keeps the marshalling to the one case every driver
agrees on.
-}
sideStoryParam :: PersistValue
sideStoryParam =
  PersistText $ T.intercalate "," [unCardCode (unScenarioId sid) | sid <- sideStoryIds]

getApiV1AdminGameStatsR :: Handler GameStatsResponse
getApiV1AdminGameStatsR = do
  populated <- runDB viewPopulated
  lastRefresh <- runDB $ P.getBy (UniqueStatsRefreshName gameStatsRefreshName)
  let refreshedAt = arkhamStatsRefreshRefreshedAt . entityVal <$> lastRefresh
      durationMs = arkhamStatsRefreshDurationMs . entityVal <$> lastRefresh
      emptyTotals = Totals 0 0 0 0 0 0 0

  -- Achievements do not come off the view, so they are worth having even before
  -- it has been built for the first time.
  (achievements, achievementUsers) <- runDB $ (,) <$> readAchievements <*> readAchievementUsers

  if not populated
    then
      pure
        GameStatsResponse
          { gameStatsResponsePopulated = False
          , gameStatsResponseRefreshedAt = refreshedAt
          , gameStatsResponseRefreshDurationMs = durationMs
          , gameStatsResponseTotals = emptyTotals
          , gameStatsResponseCampaigns = []
          , gameStatsResponseStandalones = []
          , gameStatsResponsePlayerCounts = []
          , gameStatsResponseDifficulties = []
          , gameStatsResponseVariants = []
          , gameStatsResponseSideStoriesInCampaigns = []
          , gameStatsResponseScenarioOutcomes = []
          , gameStatsResponseCampaignProgress = []
          , gameStatsResponseAchievements = achievements
          , gameStatsResponseAchievementUsers = achievementUsers
          , gameStatsResponseMonthly = []
          , gameStatsResponseNames = codeNames
          }
    else runDB do
      totals <- readTotals
      campaigns <- readCampaigns
      standalones <- readStandalones
      playerCounts <- readPlayerCounts
      difficulties <- readDifficulties
      variants <- readVariants
      sideStories <- readSideStoriesInCampaigns
      outcomes <- readScenarioOutcomes
      progress <- readCampaignProgress
      monthly <- readMonthly
      pure
        GameStatsResponse
          { gameStatsResponsePopulated = True
          , gameStatsResponseRefreshedAt = refreshedAt
          , gameStatsResponseRefreshDurationMs = durationMs
          , gameStatsResponseTotals = fromMaybe emptyTotals (listToMaybe totals)
          , gameStatsResponseCampaigns = campaigns
          , gameStatsResponseStandalones = standalones
          , gameStatsResponsePlayerCounts = playerCounts
          , gameStatsResponseDifficulties = difficulties
          , gameStatsResponseVariants = variants
          , gameStatsResponseSideStoriesInCampaigns = sideStories
          , gameStatsResponseScenarioOutcomes = outcomes
          , gameStatsResponseCampaignProgress = progress
          , gameStatsResponseAchievements = achievements
          , gameStatsResponseAchievementUsers = achievementUsers
          , gameStatsResponseMonthly = monthly
          , gameStatsResponseNames = codeNames
          }

readTotals :: DB [Totals]
readTotals =
  queryJson
    "SELECT count(*)::int AS games, \
    \count(*) FILTER (WHERE is_campaign)::int AS \"campaignGames\", \
    \count(*) FILTER (WHERE NOT is_campaign)::int AS \"standaloneGames\", \
    \count(*) FILTER (WHERE state = 'IsOver')::int AS finished, \
    \count(*) FILTER (WHERE state = 'IsActive')::int AS \"inProgress\", \
    \count(*) FILTER (WHERE state IN ('IsPending', 'IsChooseDecks'))::int AS \"neverStarted\", \
    \(SELECT count(DISTINCT user_id)::int FROM arkham_players) AS players \
    \FROM arkham_game_stats"
    []

readCampaigns :: DB [CampaignStat]
readCampaigns =
  queryJson
    "SELECT campaign_id AS id, \
    \count(*)::int AS games, \
    \count(*) FILTER (WHERE state = 'IsOver')::int AS finished, \
    \count(*) FILTER (WHERE state = 'IsActive')::int AS \"inProgress\", \
    \count(*) FILTER (WHERE state IN ('IsPending', 'IsChooseDecks'))::int AS \"neverStarted\", \
    \COALESCE(sum(cardinality(completed_scenarios)), 0)::int AS \"scenariosPlayed\", \
    \COALESCE(sum(cardinality(resolved_scenarios)), 0)::int AS \"scenariosWon\", \
    \count(*) FILTER (WHERE player_count = 1)::int AS players1, \
    \count(*) FILTER (WHERE player_count = 2)::int AS players2, \
    \count(*) FILTER (WHERE player_count = 3)::int AS players3, \
    \count(*) FILTER (WHERE player_count >= 4)::int AS players4, \
    \count(*) FILTER (WHERE difficulty = 'Easy')::int AS easy, \
    \count(*) FILTER (WHERE difficulty = 'Standard')::int AS standard, \
    \count(*) FILTER (WHERE difficulty = 'Hard')::int AS hard, \
    \count(*) FILTER (WHERE difficulty = 'Expert')::int AS expert \
    \FROM arkham_game_stats \
    \WHERE is_campaign AND campaign_id IS NOT NULL \
    \GROUP BY campaign_id ORDER BY count(*) DESC"
    []

readStandalones :: DB [StandaloneStat]
readStandalones =
  queryJson
    "SELECT scenario_id AS id, count(*)::int AS games, \
    \count(*) FILTER (WHERE state = 'IsOver')::int AS finished, \
    \(scenario_id = ANY(string_to_array(?::text, ','))) AS \"isSideStory\" \
    \FROM arkham_game_stats \
    \WHERE NOT is_campaign AND scenario_id IS NOT NULL \
    \GROUP BY scenario_id ORDER BY count(*) DESC"
    [sideStoryParam]

{- | Player count across every game. Anything above four is folded into four: the
game does not seat more, so a bigger number is bad data rather than a category.
-}
readPlayerCounts :: DB [CountStat]
readPlayerCounts =
  queryJson
    "SELECT (CASE WHEN player_count >= 4 THEN 4 ELSE player_count END)::text AS key, \
    \count(*)::int AS count \
    \FROM arkham_game_stats WHERE player_count IS NOT NULL \
    \GROUP BY 1 ORDER BY 1"
    []

readDifficulties :: DB [CountStat]
readDifficulties =
  queryJson
    "SELECT difficulty AS key, count(*)::int AS count FROM arkham_game_stats \
    \WHERE difficulty IS NOT NULL GROUP BY 1 ORDER BY count(*) DESC"
    []

readVariants :: DB [CountStat]
readVariants =
  queryJson
    "SELECT multiplayer_variant::text AS key, count(*)::int AS count \
    \FROM arkham_game_stats GROUP BY 1 ORDER BY count(*) DESC"
    []

{- | Side stories taken inside a campaign.

A campaign's completed scenarios include any side story played along the way, so
these are its completed codes that are on the side-story list. The list comes from
the engine rather than from the data: a scenario's own @isSideStory@ flag is only
set while that scenario is loaded, and is absent entirely on games saved before
the flag existed, so it cannot be read back off a finished campaign.
-}
readSideStoriesInCampaigns :: DB [SideStoryInCampaign]
readSideStoriesInCampaigns =
  queryJson
    "SELECT s.campaign_id AS \"campaignId\", c.code AS \"scenarioId\", count(*)::int AS count \
    \FROM arkham_game_stats s \
    \CROSS JOIN LATERAL unnest(s.completed_scenarios) AS c(code) \
    \WHERE s.is_campaign AND s.campaign_id IS NOT NULL \
    \AND c.code = ANY(string_to_array(?::text, ',')) \
    \GROUP BY 1, 2 ORDER BY 3 DESC"
    [sideStoryParam]

{- | Per scenario, how many campaign runs finished it and how many resolved it.

Campaign games only. A standalone keeps no resolutions map, so counting it here
would mix "we know how this went" with "we do not".
-}
readScenarioOutcomes :: DB [ScenarioOutcome]
readScenarioOutcomes =
  queryJson
    "SELECT c.code AS id, count(*)::int AS played, \
    \count(*) FILTER (WHERE c.code = ANY(s.resolved_scenarios))::int AS won \
    \FROM arkham_game_stats s \
    \CROSS JOIN LATERAL unnest(s.completed_scenarios) AS c(code) \
    \WHERE s.is_campaign \
    \GROUP BY 1 ORDER BY 2 DESC"
    []

-- | How many campaign runs have completed exactly N scenarios.
readCampaignProgress :: DB [CampaignProgress]
readCampaignProgress =
  queryJson
    "SELECT campaign_id AS \"campaignId\", \
    \cardinality(completed_scenarios)::int AS scenarios, \
    \count(*)::int AS games \
    \FROM arkham_game_stats \
    \WHERE is_campaign AND campaign_id IS NOT NULL \
    \GROUP BY 1, 2 ORDER BY 1, 2"
    []

{- | Read live rather than off the view: the table is already relational and the
partial index on earned rows makes this a cheap grouped scan, so these numbers may
as well be current.
-}
readAchievements :: DB [AchievementStat]
readAchievements =
  queryJson
    "SELECT achievement::text AS id, \
    \count(*) FILTER (WHERE earned_at IS NOT NULL)::int AS earned, \
    \count(*) FILTER (WHERE earned_at IS NULL)::int AS \"inProgress\" \
    \FROM arkham_achievements GROUP BY 1 \
    \ORDER BY count(*) FILTER (WHERE earned_at IS NOT NULL) DESC, 1"
    []

readAchievementUsers :: DB Int
readAchievementUsers = do
  rows :: [Single Int] <-
    rawSql
      "SELECT count(DISTINCT user_id)::int FROM arkham_achievements WHERE earned_at IS NOT NULL"
      []
  pure $ case rows of
    Single n : _ -> n
    [] -> 0

-- | Games started per month over the last two years, oldest first.
readMonthly :: DB [CountStat]
readMonthly =
  queryJson
    "SELECT to_char(date_trunc('month', created_at), 'YYYY-MM') AS key, \
    \count(*)::int AS count \
    \FROM arkham_game_stats \
    \WHERE created_at >= date_trunc('month', now()) - interval '23 months' \
    \GROUP BY 1 ORDER BY 1"
    []

-- ------------------------------------------------------------------- refresh ---

{- | Rebuild the view.

Not @CONCURRENTLY@: that cannot run inside a transaction, and every handler here
runs in one. The plain form takes an exclusive lock on the view while it rebuilds,
which is acceptable because nothing but this panel reads it -- the game does not
know it exists. If the rebuild ever grows long enough that one admin refreshing
blocks another reading, the unique index is already there to switch over.

The elapsed time is recorded next to the timestamp, so whoever decides how often
this should run can see what it costs.
-}
postApiV1AdminGameStatsRefreshR :: Handler GameStatsResponse
postApiV1AdminGameStatsRefreshR = do
  started <- liftIO getCurrentTime
  runDB $ rawExecute "REFRESH MATERIALIZED VIEW arkham_game_stats" []
  finished <- liftIO getCurrentTime
  let elapsed = round (realToFrac (diffUTCTime finished started) * 1000 :: Double)
  runDB do
    existing <- P.getBy (UniqueStatsRefreshName gameStatsRefreshName)
    case existing of
      Just (Entity rowId _) ->
        P.update
          rowId
          [ ArkhamStatsRefreshRefreshedAt P.=. finished
          , ArkhamStatsRefreshDurationMs P.=. elapsed
          ]
      Nothing -> P.insert_ $ ArkhamStatsRefresh gameStatsRefreshName finished elapsed
  getApiV1AdminGameStatsR

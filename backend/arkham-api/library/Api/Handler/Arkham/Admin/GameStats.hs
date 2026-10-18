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
  postApiV1AdminGameStatsOutcomesR,
) where

import Arkham.Campaign (allCampaigns, lookupCampaign)
import Arkham.Campaign.Types (campaignName)
import Arkham.CampaignStep (CampaignStep)
import Arkham.Card.CardCode (CardCode (..))
import Arkham.Card.CardDef (cdName)
import Arkham.Classes.Entity (toAttrs)
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Game.Outcome
import Arkham.Id (CampaignId (..), ScenarioId (..))
import Arkham.Name (display)
import Arkham.Resolution (Resolution (..))
import Arkham.Scenario (allScenarioCards)
import Arkham.SideStory (sideStoryIds)
import Data.Aeson.Types (parseEither)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock
import Data.UUID (UUID)
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
  , totalsWon :: Int
  -- ^ Finished games whose outcome is a win. See "Arkham.Game.Outcome".
  , totalsWonUncertain :: Int
  {- ^ Of the wins, how many the Campaign Guide prints as "The investigators win
  the campaign...?" rather than stating outright.
  -}
  , totalsLost :: Int
  , totalsOutcomeUnknown :: Int
  {- ^ Finished, but with no outcome recorded. Games that ended before outcomes
  were tracked, until the outcomes backfill has run over them.
  -}
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
  , campaignStatWon :: Int
  {- ^ Runs that reached a winning ending; @finished@ minus this and @lost@ is
  how many finished without recording an outcome.
  -}
  , campaignStatLost :: Int
  , campaignStatOutcomeUnknown :: Int
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
  , standaloneStatWon :: Int
  , standaloneStatLost :: Int
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

{- | Which ending a campaign's runs actually stopped on.

The coarse won/lost split hides what Arkham spends most of its endings on: a
campaign usually has several, and several of those are neither a clean win nor a
wipe. @resolution@ is 'Nothing' for a scenario lost or resigned out of
(@NoResolution@), which is the one ending that needs no number.
-}
data CampaignEnding = CampaignEnding
  { campaignEndingCampaignId :: Text
  , campaignEndingScenarioId :: Text
  , campaignEndingResolution :: Maybe Int
  , campaignEndingResult :: Text
  -- ^ 'Won' or 'Lost', as the outcome recorded it.
  , campaignEndingCertainty :: Text
  -- ^ 'Certain', or 'Uncertain' for a win the guide will not commit to.
  , campaignEndingNote :: Maybe Text
  -- ^ i18n key for the clause the guide attaches to the verdict, if any.
  , campaignEndingCount :: Int
  }
  deriving stock Generic

instance ToJSON CampaignEnding where
  toJSON = genericToJSON $ aesonOptions $ Just "campaignEnding"
  toEncoding = genericToEncoding $ aesonOptions $ Just "campaignEnding"

instance FromJSON CampaignEnding where
  parseJSON = genericParseJSON $ aesonOptions $ Just "campaignEnding"

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
  , gameStatsResponseCampaignEndings :: [CampaignEnding]
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
      emptyTotals = Totals 0 0 0 0 0 0 0 0 0 0 0

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
          , gameStatsResponseCampaignEndings = []
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
      endings <- readCampaignEndings
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
          , gameStatsResponseCampaignEndings = endings
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
    \(SELECT count(DISTINCT user_id)::int FROM arkham_players) AS players, \
    \count(*) FILTER (WHERE outcome_result = 'Won')::int AS won, \
    \count(*) FILTER (WHERE outcome_result = 'Won' AND outcome_certainty = 'Uncertain')::int \
    \  AS \"wonUncertain\", \
    \count(*) FILTER (WHERE outcome_result = 'Lost')::int AS lost, \
    \count(*) FILTER (WHERE state = 'IsOver' AND outcome_result IS NULL)::int \
    \  AS \"outcomeUnknown\" \
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
    \count(*) FILTER (WHERE outcome_result = 'Won')::int AS won, \
    \count(*) FILTER (WHERE outcome_result = 'Lost')::int AS lost, \
    \count(*) FILTER (WHERE state = 'IsOver' AND outcome_result IS NULL)::int \
    \  AS \"outcomeUnknown\", \
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
    \count(*) FILTER (WHERE outcome_result = 'Won')::int AS won, \
    \count(*) FILTER (WHERE outcome_result = 'Lost')::int AS lost, \
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

{- | Which endings campaign runs actually stopped on, most common first.

Only games with a recorded outcome appear: a run that ended before outcomes were
tracked has no ending to attribute, and counting it under a guess would make this
the least trustworthy table on the page rather than the most specific one.
-}
readCampaignEndings :: DB [CampaignEnding]
readCampaignEndings =
  queryJson
    "SELECT campaign_id AS \"campaignId\", \
    \outcome_scenario AS \"scenarioId\", \
    \outcome_resolution AS resolution, \
    \outcome_result AS result, \
    \outcome_certainty AS certainty, \
    \outcome_note AS note, \
    \count(*)::int AS count \
    \FROM arkham_game_stats \
    \WHERE is_campaign AND campaign_id IS NOT NULL \
    \  AND outcome_result IS NOT NULL AND outcome_scenario IS NOT NULL \
    \GROUP BY 1, 2, 3, 4, 5, 6 ORDER BY count(*) DESC, 1, 2, 3"
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

-- ------------------------------------------------------------------ backfill ---

{- | What one pass of the outcomes backfill did.

@remaining@ is what is left after this pass, so a panel can keep pressing until
it reaches zero rather than guessing how many passes a database needs.
-}
data OutcomesResponse = OutcomesResponse
  { outcomesResponseComputed :: Int
  -- ^ Games this pass worked out a verdict for.
  , outcomesResponseUndecided :: Int
  {- ^ Games this pass looked at and could not judge -- a campaign abandoned
  between scenarios with nothing resolved. They are left alone rather than
  recorded as a loss, and will be looked at again on the next pass.
  -}
  , outcomesResponseRemaining :: Int
  , outcomesResponseDurationMs :: Int
  }
  deriving stock Generic

instance ToJSON OutcomesResponse where
  toJSON = genericToJSON $ aesonOptions $ Just "outcomesResponse"
  toEncoding = genericToEncoding $ aesonOptions $ Just "outcomesResponse"

{- | Just enough of a finished game to work out how it ended.

Read as six narrow jsonb paths rather than by parsing the game: @fromJSON \@Game@
reconstructs the entire engine state per row, and all that is wanted here is a
campaign's step, its completed steps and its resolutions.
-}
data OutcomeCandidate = OutcomeCandidate
  { candidateGameId :: UUID
  , candidateCampaignId :: Maybe CampaignId
  , candidateStep :: Maybe CampaignStep
  , candidateCompletedSteps :: [CampaignStep]
  , candidateResolutions :: Map ScenarioId Resolution
  , candidateLastResolution :: Maybe Ending
  }
  deriving stock Generic

instance FromJSON OutcomeCandidate where
  parseJSON = withObject "OutcomeCandidate" \o ->
    OutcomeCandidate
      <$> o
      .: "gameId"
      <*> o
      .:? "campaignId"
      <*> o
      .:? "step"
      <*> o
      .:? "completedSteps"
      .!= []
      <*> o
      .:? "resolutions"
      .!= mempty
      <*> o
      .:? "lastResolution"

{- | How many games one pass looks at.

Each row detoasts a ~28kB blob to read six paths out of it, so a pass is bounded
and the panel repeats it. Big enough that a few presses clear a typical database,
small enough that a pass cannot hold a request open long enough to be killed.
-}
outcomesBatchSize :: Int
outcomesBatchSize = 500

{- | Work out how finished games ended, for the ones the engine never recorded.

Only games with no recorded outcome and no row of their own are considered, so a
pass is idempotent and re-running it costs nothing but the scan. A game it cannot
judge is skipped rather than guessed at, which means a pass can report fewer
computed than it looked at and 'outcomesResponseRemaining' can stop falling; that
is the honest end state, not a stall.
-}
postApiV1AdminGameStatsOutcomesR :: Handler OutcomesResponse
postApiV1AdminGameStatsOutcomesR = do
  started <- liftIO getCurrentTime
  (computed, undecided) <- runDB do
    candidates <- readOutcomeCandidates outcomesBatchSize
    let judged = [(c, o) | c <- candidates, Just o <- [judge c]]
    for_ judged \(c, o) -> insertOutcome started c o
    pure (length judged, length candidates - length judged)
  remaining <- runDB countOutcomeCandidates
  finished <- liftIO getCurrentTime
  pure
    OutcomesResponse
      { outcomesResponseComputed = computed
      , outcomesResponseUndecided = undecided
      , outcomesResponseRemaining = remaining
      , outcomesResponseDurationMs =
          round (realToFrac (diffUTCTime finished started) * 1000 :: Double)
      }
 where
  judge c = case candidateCampaignId c of
    Just cid -> do
      step <- candidateStep c
      o <- inferCampaignOutcome cid step (candidateCompletedSteps c) (candidateResolutions c)
      -- The resolution the game itself last saw beats the one reconstructed from
      -- the campaign's records, which cannot say which scenario was last.
      pure o {outcomeEnding = candidateLastResolution c <|> outcomeEnding o}
    -- No campaign: a standalone scenario, where the resolution IS the ending.
    Nothing -> standaloneOutcome <$> candidateLastResolution c

{- | Finished games with no outcome from either source.

@jsonb_typeof(...) IS DISTINCT FROM 'object'@ rather than @IS NULL@: a game saved
while it was still running carries @"gameOutcome": null@, which @->@ returns as
JSON null and not as SQL NULL.
-}
outcomeCandidateFilter :: Text
outcomeCandidateFilter =
  "FROM arkham_games g \
  \LEFT JOIN arkham_game_outcomes o ON o.arkham_game_id = g.id \
  \WHERE g.current_data #>> '{gameGameState,tag}' = 'IsOver' \
  \  AND o.id IS NULL \
  \  AND jsonb_typeof(g.current_data -> 'gameOutcome') IS DISTINCT FROM 'object' "

readOutcomeCandidates :: Int -> DB [OutcomeCandidate]
readOutcomeCandidates limit =
  queryJson
    ( "SELECT g.id AS \"gameId\", \
      \g.current_data #> '{gameMode,This,id}' AS \"campaignId\", \
      \g.current_data #> '{gameMode,This,step}' AS step, \
      \CASE WHEN jsonb_typeof(g.current_data #> '{gameMode,This,completedSteps}') = 'array' \
      \     THEN g.current_data #> '{gameMode,This,completedSteps}' \
      \     ELSE '[]'::jsonb END AS \"completedSteps\", \
      \CASE WHEN jsonb_typeof(g.current_data #> '{gameMode,This,resolutions}') = 'object' \
      \     THEN g.current_data #> '{gameMode,This,resolutions}' \
      \     ELSE '{}'::jsonb END AS resolutions, \
      \g.current_data #> '{gameLastResolution}' AS \"lastResolution\" "
        <> outcomeCandidateFilter
        <> "ORDER BY g.updated_at DESC LIMIT ?"
    )
    [toPersistValue limit]

countOutcomeCandidates :: DB Int
countOutcomeCandidates = do
  rows :: [Single Int] <- rawSql ("SELECT count(*)::int " <> outcomeCandidateFilter) []
  pure $ case rows of
    Single n : _ -> n
    [] -> 0

insertOutcome :: UTCTime -> OutcomeCandidate -> GameOutcome -> DB ()
insertOutcome now c o =
  rawExecute
    "INSERT INTO arkham_game_outcomes \
    \(arkham_game_id, result, certainty, note, source, scenario_id, resolution_tag, \
    \ resolution, computed_at) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT (arkham_game_id) DO NOTHING"
    [ toPersistValue (candidateGameId c)
    , toPersistValue (tshow (verdictResult (outcomeVerdict o)))
    , toPersistValue (tshow (verdictCertainty (outcomeVerdict o)))
    , toPersistValue (verdictNote (outcomeVerdict o))
    , toPersistValue (tshow (outcomeSource o))
    , toPersistValue (unCardCode . unScenarioId . endingScenario <$> outcomeEnding o)
    , toPersistValue (resolutionTag . endingResolution <$> outcomeEnding o)
    , toPersistValue (resolutionNumber . endingResolution =<< outcomeEnding o)
    , toPersistValue now
    ]
 where
  resolutionTag = \case
    NoResolution -> "NoResolution" :: Text
    Resolution _ -> "Resolution"
  resolutionNumber = \case
    NoResolution -> Nothing
    Resolution n -> Just n

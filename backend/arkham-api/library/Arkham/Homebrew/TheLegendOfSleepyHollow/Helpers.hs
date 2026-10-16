{- | Shared rules for The Legend of Sleepy Hollow.

Two things run through the whole scenario. Doom on *locations* is the agenda's
clock -- advancing never clears it, and most of the encounter set reads whether
your location has doom on it. And the nine memories on act 2 are what buy the
investigators their confrontation with the Horseman; every one of them is
remembered by a different card.
-}
module Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers where

import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Distance (unDistance)
import Arkham.GameEnv (getDistance)
import Arkham.GameValue
import Arkham.Helpers.SkillTest (getSkillTestResultWithResultModifiers)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Locations qualified as Locations
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Types (Field (LocationDoom))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.ScenarioLogKey (ScenarioLogKey (..))
import Arkham.SkillTestResult (SkillTestResult (FailedBy))

memory :: Text -> ScenarioLogKey
memory = HomebrewScenarioLogKey . ("theLegendOfSleepyHollow." <>)

gazedIntoTheHorsemansSoul :: ScenarioLogKey
gazedIntoTheHorsemansSoul = memory "GazedIntoTheHorsemansSoul"

recoveredTheHorsemansSkull :: ScenarioLogKey
recoveredTheHorsemansSkull = memory "RecoveredTheHorsemansSkull"

learnedTheHorsemansName :: ScenarioLogKey
learnedTheHorsemansName = memory "LearnedTheHorsemansName"

studiedTheHorsemansPath :: ScenarioLogKey
studiedTheHorsemansPath = memory "StudiedTheHorsemansPath"

interviewedVanBruntsStaff :: ScenarioLogKey
interviewedVanBruntsStaff = memory "InterviewedVanBruntsStaff"

readTheHollowsHistory :: ScenarioLogKey
readTheHollowsHistory = memory "ReadTheHollowsHistory"

interrogatedAnElderWitch :: ScenarioLogKey
interrogatedAnElderWitch = memory "InterrogatedAnElderWitch"

listenedToAVictimsStory :: ScenarioLogKey
listenedToAVictimsStory = memory "ListenedToAVictimsStory"

performedADivinationSpell :: ScenarioLogKey
performedADivinationSpell = memory "PerformedADivinationSpell"

-- | The nine bullets act 2 counts, in printed order.
horsemansMemories :: [ScenarioLogKey]
horsemansMemories =
  [ gazedIntoTheHorsemansSoul
  , recoveredTheHorsemansSkull
  , learnedTheHorsemansName
  , studiedTheHorsemansPath
  , interviewedVanBruntsStaff
  , readTheHollowsHistory
  , interrogatedAnElderWitch
  , listenedToAVictimsStory
  , performedADivinationSpell
  ]

headlessHorseman :: EnemyMatcher
headlessHorseman = enemyIs Enemies.theHeadlessHorseman

{- | "While Disembodied Skull is at a location, that location is treated as if
it had doom on it while resolving enemy and treachery effects." Only the enemy
and treachery cards that ask about doom read through this; the location
abilities and the scenario reference's own tokens count real doom.
-}
locationIsDoomed :: HasGame m => LocationId -> m Bool
locationIsDoomed lid =
  orM
    [ (> 0) <$> field LocationDoom lid
    , selectAny $ enemyIs Enemies.disembodiedSkull <> enemyAt lid
    ]

{- | "the location that is closest to Wiley's Swamp that does not already have
doom on it, other than Old Dutch Church". Ties are left to the lead to settle;
the swamp itself is its own distance 0.
-}
closestUndoomedToWileysSwamp :: HasGame m => m [LocationId]
closestUndoomedToWileysSwamp = do
  selectOne (locationIs Locations.wileysSwamp) >>= \case
    Nothing -> pure []
    Just swamp -> do
      candidates <-
        select $ LocationWithDoom (EqualTo $ Static 0) <> not_ (locationIs Locations.oldDutchChurch)
      ranked <- forMaybeM candidates \lid ->
        if lid == swamp
          then pure $ Just (0 :: Int, lid)
          else fmap ((,lid) . unDistance) <$> getDistance swamp lid
      pure $ case sortOn fst ranked of
        [] -> []
        (d, _) : _ -> [lid | (n, lid) <- ranked, n == d]

{- | How much the current test was failed by, or 0 outside a test -- which is
what Looming Spirit's "resolve that haunted ability at 0 skill value" asks for.
-}
amountFailedBy :: HasGame m => m Int
amountFailedBy =
  getSkillTestResultWithResultModifiers <&> \case
    Just (FailedBy _ n) -> n
    _ -> 0

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "theLegendOfSleepyHollow" a

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "scenario" a

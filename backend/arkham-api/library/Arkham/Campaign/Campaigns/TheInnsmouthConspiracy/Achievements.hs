{- | The Innsmouth Conspiracy achievement detection. Hooked from the campaign's
runMessage (campaign dispatch runs for every message, BEFORE the scenario and
other entities, so defeated enemies etc. are still queryable).
'earnAchievement' self-gates on the achievements setting and on the campaign
id ("07"), so earns stay unconditional here.

Campaign-persistent trackers live in the campaign store (via 'SetGlobal'
messages, read back with 'stored'); the whole module is additionally gated to
achievement-eligible campaigns, matching every other campaign's detection
module. Like The Drowned City — and unlike the Return-to lists — this list is
printed for the campaign itself, so there is no separate variant to inherit
the hook.

Scenario-end detections key on 'EndOfGame', NOT on 'ScenarioResolution': the
Scenario wrapper clearQueues twice while processing a resolution, wiping even
Priority pushes made during that dispatch. 'EndOfGame' is what 'endOfScenario'
pushes afterwards, and the board is still fully intact when it dispatches
(teardown happens at the 'EndOfScenario' it queues), so controlled assets can
still be read. It doubles as the "completed the scenario" signal: the losing
endings that wipe the party (A Light in the Fog R3, Into the Maelstrom R6-R8)
call 'gameOver' instead and never push it.
-}
module Arkham.Campaign.Campaigns.TheInnsmouthConspiracy.Achievements (
  runInnsmouthConspiracyAchievements,
) where

import Arkham.Achievement
import Arkham.Act.Cards qualified as Acts
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types qualified as Asset
import Arkham.Campaign.Types (campaignDifficulty)
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.Card
import Arkham.Classes.Entity (toAttrs)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Difficulty
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types qualified as Enemy
import Arkham.Game.Base
import Arkham.Game.Settings (activeUltimatumsAndBoons)
import Arkham.Helpers.Campaign (stored)
import Arkham.Helpers.Log (getRecordSet)
import Arkham.Helpers.Scenario (scenarioFieldMap)
import Arkham.Id
import Arkham.Investigator.Types qualified as Investigator
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types qualified as Location
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (ScenarioMeta))
import Arkham.ScenarioLogKey (ScenarioCountKey (Barriers))
import Arkham.Scenarios.InTooDeep.Helpers qualified as InTooDeep
import Arkham.Target
import Arkham.Tracing
import Arkham.Trait (Trait (DeepOne, Vehicle))
import Arkham.UltimatumsAndBoons.Types
import Data.Aeson.Key qualified as Key
import Data.Map.Strict qualified as Map

runInnsmouthConspiracyAchievements
  :: (HasGame m, HasQueue Message m, Tracing m) => Message -> m ()
runInnsmouthConspiracyAchievements msg = whenEligibleCampaign $ case msg of
  -- Enemy defeats. The campaign sees Defeated before the enemy processes it, and
  -- the message already carries the enemy's traits, so nothing has to be looked
  -- up for the trait-based tallies.
  Defeated (EnemyTarget eid) _ _ traits -> do
    -- "Would You Just Die Already": The Amalgam is never discarded — its forced
    -- ability puts it back in The Depths with its damage cleared — so each kill
    -- produces another Defeated.
    whenScenarioIs thePitOfDespairId do
      cardDef <- fieldMap Enemy.EnemyCard toCardDef eid
      when (cardDef == Enemies.theAmalgam) do
        n <- storedInt amalgamDefeatsKey
        setStore amalgamDefeatsKey (n + 1)
        when (n + 1 >= 5) $ earn WouldYouJustDieAlready

    -- "Gone Fishing" (20 in a campaign) and, by its absence at the epilogue,
    -- "Bigger Fish to Fry". Both are campaign-wide tallies, so no scenario gate.
    when (DeepOne `elem` traits) do
      n <- storedInt deepOnesDefeatedKey
      setStore deepOnesDefeatedKey (n + 1)
      when (n + 1 >= 20) $ earn GoneFishing

  {- "Elementary, Dear Dawson": The Search for Agent Harper asks the lead to name
  the suspect and then the hideout as it advances, and defers a DoStep 1 for each
  correct guess. Both questions are answered (and so both DoSteps dispatched)
  before the act's own DoStep 2 runs, but counting the DoSteps here rather than
  reading the act's meta keeps the detection independent of how the act stores
  its tally.
  -}
  DoStep 1 (AdvanceAct aid _ _)
    | unActId aid == toCardCode Acts.theSearchForAgentHarper ->
        whenScenarioIs theVanishingOfElinaHarperId do
          n <- storedInt correctGuessesKey
          setStore correctGuessesKey (n + 1)
          when (n + 1 >= 2) $ earn ElementaryDearDawson

  {- "Ain't Nothin Gonna Break My Stride": every barrier destroyed. The barrier
  counts live in In Too Deep's scenario meta, which the scenario updates when it
  processes this same message — i.e. after the campaign has already seen it — so
  the decrement is applied to a local copy here using the scenario's own helper.
  -}
  ScenarioCountDecrementBy (Barriers l1 l2) n -> whenScenarioIs inTooDeepId do
    meta <- scenarioFieldMap ScenarioMeta (toResultDefault $ InTooDeep.Meta mempty)
    let InTooDeep.Meta barriers = InTooDeep.decrementBarriers n l1 l2 meta
    when (notNull barriers && all (<= 0) (Map.elems barriers)) do
      earn AintNothinGonnaBreakMyStride

  {- "Speeding Ticket" bookkeeping. Any of the three disqualifiers latches a flag
  that is only cleared when Horror in High Gear is set up.

  Stopping: the running cars' only Flip pushes ReplaceAsset with the Stopped
  side, and nothing else in the scenario stops a car, so this IS the voluntary
  stop.
  -}
  ReplaceAsset _ def | def `elem` stoppedCars -> whenScenarioIs horrorInHighGearId do
    setStore speedingTicketBrokenKey True
  -- Getting out: the exit ability re-places the investigator at a location. The
  -- campaign sees this before the placement changes, so a still-InVehicle
  -- placement means they are leaving one. Entering a Long Way Around on foot is
  -- caught here too.
  PlaceInvestigator iid (AtLocation lid) -> whenScenarioIs horrorInHighGearId do
    placement <- field Investigator.InvestigatorPlacement iid
    case placement of
      InVehicle _ -> setStore speedingTicketBrokenKey True
      _ -> pure ()
    whenM (isLongWayAround lid) $ setStore speedingTicketBrokenKey True
  -- Entering a Long Way Around: the vehicle is re-placed at the new road.
  PlaceAsset aid (AtLocation lid) -> whenScenarioIs horrorInHighGearId do
    isVehicle <- aid <=~> AssetWithTrait Vehicle
    when isVehicle $ whenM (isLongWayAround lid) $ setStore speedingTicketBrokenKey True
  {- The earn. Falcon Point Approach's objective ability is the only thing that
  advances Pedal to the Metal, so its advance IS "reached Falcon Point Approach"
  (the scenario is otherwise lost on the agenda). AdvanceAct dispatches for both
  sides; the duplicate earn is harmless.
  -}
  AdvanceAct aid _ _ | unActId aid == toCardCode Acts.pedalToTheMetal ->
    whenScenarioIs horrorInHighGearId do
      unlessM (storedFlag speedingTicketBrokenKey) $ earn SpeedingTicket

  -- "You're Locked In Here With Me" bookkeeping: A Light in the Fog flags a
  -- captured investigator by dispatching its own per-investigator message.
  ForInvestigator _ (ScenarioSpecific "captured" _) -> whenScenarioIs aLightInTheFogId do
    setStore capturedKey True

  {- "Don't Wake Daddy" bookkeeping. Both gods wake by flipping to their Awakened
  and Enraged side, which swaps in the awakened card; Dagon additionally counts as
  awake whenever the campaign log says so (the ritual completing, or the scenario
  being lost, wakes him without a flip).
  -}
  ReplaceEnemy _ card _ | toCardDef card `elem` awakenedGods -> setStore dagonOrHydraAwakeKey True
  Record key | key == toCampaignLogKey DagonHasAwakened -> setStore dagonOrHydraAwakeKey True
  {- "Full Build": one investigator holding all three Devil Reef relics at once.
  Checked as the third arrives, since the campaign runs before the asset does and
  so cannot see the newcomer in play yet — only the two already there.
  -}
  CardEnteredPlay iid card -> checkFullBuild iid (toCardDef card)
  TakeControlOfAsset iid aid -> checkFullBuild iid =<< fieldMap Asset.AssetCard toCardDef aid
  -- Per-game counters and flags reset as their scenario is set up, so a revisited
  -- scenario cannot inherit a previous game's tally.
  Setup -> do
    whenScenarioIs thePitOfDespairId $ setStore amalgamDefeatsKey (0 :: Int)
    whenScenarioIs theVanishingOfElinaHarperId $ setStore correctGuessesKey (0 :: Int)
    whenScenarioIs horrorInHighGearId $ setStore speedingTicketBrokenKey False
    whenScenarioIs aLightInTheFogId $ setStore capturedKey False

  -- Scenario-completion detections. See the module header for why these hang off
  -- EndOfGame rather than the resolution.
  EndOfGame _ ->
    selectOne TheScenario >>= traverse_ \sid ->
      if
        | sid == aLightInTheFogId ->
            -- "You're Locked In Here With Me": nobody was ever captured.
            unlessM (storedFlag capturedKey) $ earn YoureLockedInHereWithMe
        | sid == theLairOfDagonId ->
            -- Half of "Don't Wake Daddy": finished the Lair with Dagon asleep.
            unlessM (storedFlag dagonOrHydraAwakeKey) $ setStore lairOfDagonUndisturbedKey True
        | sid == intoTheMaelstromId -> do
            -- "Don't Wake Daddy": and finished the Maelstrom with both asleep.
            awake <- storedFlag dagonOrHydraAwakeKey
            lairClean <- storedFlag lairOfDagonUndisturbedKey
            when (lairClean && not awake) $ earn DontWakeDaddy

            {- "Fish Out of Water": every investigator still has the Diving Suit
            they earned by resigning in the Moon Room. Eliminated investigators are
            included deliberately — a defeated investigator's suit is discarded
            with the rest of their cards, which is exactly not "still wearing" it.
            -}
            investigators <- select $ IncludeEliminated Anyone
            suited <- filterM hasDivingSuit investigators
            when (notNull investigators && length suited == length investigators) do
              earn FishOutOfWater
        | otherwise -> pure ()
  -- End of campaign. Every surviving ending routes through the epilogue; the
  -- endings that wipe or doom the party call gameOver instead and never get here,
  -- so reaching this step is both "completed" and "won".
  CampaignStep EpilogueStep -> do
    g <- getGame
    let difficulty = campaignDifficulty . toAttrs <$> currentCampaign (gameMode g)

    -- "Bigger Fish to Fry": finished without defeating a single Deep One.
    defeatedDeepOnes <- storedInt deepOnesDefeatedKey
    when (defeatedDeepOnes == 0) $ earn BiggerFishToFry

    -- "Line in the Sand": win with at least 3 Ultimatums active.
    let ultimatums = length [u | Ultimatum u <- toList $ activeUltimatumsAndBoons (gameSettings g)]
    when (ultimatums >= 3) $ earn InnsmouthLineInTheSand

    -- "Innsmouth Expertise": win on Expert.
    when (difficulty == Just Expert) $ earn InnsmouthExpertise

    {- '"You Wake Up In A Room..."': the flashback checklist, accumulated across
    playthroughs by the API layer. The Horrible Truth is recorded by the epilogue
    itself — i.e. after this dispatch — so it is derived the same way the epilogue
    derives it, from having recovered all fourteen memories.
    -}
    recovered <- getRecordSet MemoriesRecovered
    let found = [key | (memory, key) <- memoryItems, recorded memory `elem` recovered]
    achievementProgress (TheInnsmouthConspiracyAchievement YouWakeUpInARoom)
      $ found
      <> ["TheHorribleTruth" | length found == length memoryItems]
  _ -> pure ()

earn :: (HasGame m, HasQueue Message m) => TheInnsmouthConspiracyAchievement -> m ()
earn = earnAchievement . TheInnsmouthConspiracyAchievement

{- | Gate the whole module (including store writes) to campaigns that can earn
these achievements. Derived from 'achievementCampaigns' so this cannot drift
from 'earnAchievement''s own campaign gate.
-}
whenEligibleCampaign :: HasGame m => m () -> m ()
whenEligibleCampaign body = do
  mCampaignId <- currentCampaignId
  let eligible = achievementCampaigns $ TheInnsmouthConspiracyAchievement GoneFishing
  when (maybe False (`elem` eligible) mCampaignId) body

whenScenarioIs :: (HasGame m, Tracing m) => ScenarioId -> m () -> m ()
whenScenarioIs sid body = do
  mSid <- selectOne TheScenario
  when (mSid == Just sid) body

thePitOfDespairId, theVanishingOfElinaHarperId, inTooDeepId :: ScenarioId
thePitOfDespairId = "07041"
theVanishingOfElinaHarperId = "07056"
inTooDeepId = "07123"

horrorInHighGearId, aLightInTheFogId, theLairOfDagonId, intoTheMaelstromId :: ScenarioId
horrorInHighGearId = "07198"
aLightInTheFogId = "07231"
theLairOfDagonId = "07274"
intoTheMaelstromId = "07311"

-- | The Stopped side of each chase car; being replaced by one is a voluntary stop.
stoppedCars :: [CardDef]
stoppedCars = [Assets.thomasDawsonsCarStopped, Assets.elinaHarpersCarStopped]

{- | The awakened sides of Dagon and Hydra. Dagon has a separate printing for Into
the Maelstrom, so both of his are listed.
-}
awakenedGods :: [CardDef]
awakenedGods =
  [ Enemies.dagonAwakenedAndEnraged
  , Enemies.dagonAwakenedAndEnragedIntoTheMaelstrom
  , Enemies.hydraAwakenedAndEnraged
  ]

-- | The three Devil Reef relics "Full Build" wants on one investigator at once.
relics :: [CardDef]
relics = [Assets.wavewornIdol, Assets.awakenedMantle, Assets.headdressOfYhaNthlei]

isLongWayAround :: (HasGame m, Tracing m) => LocationId -> m Bool
isLongWayAround lid = do
  cardDef <- fieldMap Location.LocationCard toCardDef lid
  pure $ cardDef == Locations.longWayAround

hasDivingSuit :: (HasGame m, Tracing m) => InvestigatorId -> m Bool
hasDivingSuit iid =
  selectAny $ assetIs Assets.divingSuit <> AssetControlledBy (InvestigatorWithId iid)

{- | Earn "Full Build" when the relic just arriving completes the set for its
controller. Only the other two are looked up: this one is not in play yet.
-}
checkFullBuild
  :: (HasGame m, HasQueue Message m, Tracing m) => InvestigatorId -> CardDef -> m ()
checkFullBuild iid cardDef = when (cardDef `elem` relics) do
  let others = filter (/= cardDef) relics
  whenM (allM (\def -> selectAny $ assetIs def <> AssetControlledBy (InvestigatorWithId iid)) others) do
    earn FullBuild

{- | Each flashback memory paired with its checklist item key, in
'achievementChecklist' order. The Horrible Truth is not a memory — it is the
record the epilogue writes once all fourteen are recovered — so it is not here.
-}
memoryItems :: [(Memory, Text)]
memoryItems =
  [ (AMeetingWithThomasDawson, "AMeetingWithThomasDawson")
  , (ABattleWithAHorrifyingDevil, "ABattleWithAHorrifyingDevil")
  , (ADecisionToStickTogether, "ADecisionToStickTogether")
  , (AnEncounterWithASecretCult, "AnEncounterWithASecretCult")
  , (ADealWithJoeSargent, "ADealWithJoeSargent")
  , (AFollowedLead, "AFollowedLead")
  , (AnIntervention, "AnIntervention")
  , (AJailbreak, "AJailbreak")
  , (DiscoveryOfAStrangeIdol, "DiscoveryOfAStrangeIdol")
  , (DiscoveryOfAnUnholyMantle, "DiscoveryOfAnUnholyMantle")
  , (DiscoveryOfAMysticalRelic, "DiscoveryOfAMysticalRelic")
  , (AConversationWithMrMoore, "AConversationWithMrMoore")
  , (TheLifecycleOfADeepOne, "TheLifecycleOfADeepOne")
  , (AStingingBetrayal, "AStingingBetrayal")
  ]

-- Campaign store plumbing. Writes go through the queue ('SetGlobal' is handled
-- by the campaign runner); reads see all previously processed writes.

amalgamDefeatsKey
  , deepOnesDefeatedKey
  , correctGuessesKey
  , speedingTicketBrokenKey
  , capturedKey
  , dagonOrHydraAwakeKey
  , lairOfDagonUndisturbedKey
    :: Text
amalgamDefeatsKey = "ticAchAmalgamDefeats"
deepOnesDefeatedKey = "ticAchDeepOnesDefeated"
correctGuessesKey = "ticAchCorrectGuesses"
speedingTicketBrokenKey = "ticAchSpeedingTicketBroken"
capturedKey = "ticAchCaptured"
dagonOrHydraAwakeKey = "ticAchDagonOrHydraAwake"
lairOfDagonUndisturbedKey = "ticAchLairOfDagonUndisturbed"

-- Priority so the write is applied before the rest of the triggering message's
-- cascade — some cascades (e.g. defeating a victory enemy) clearQueue, which
-- would otherwise drop a plainly-pushed store write.
setStore :: (HasQueue Message m, ToJSON a) => Text -> a -> m ()
setStore k v = push $ Priority $ SetGlobal CampaignTarget (Key.fromText k) (toJSON v)

storedInt :: (HasCallStack, HasGame m, Tracing m) => Text -> m Int
storedInt k = fromMaybe 0 <$> stored k

storedFlag :: (HasCallStack, HasGame m, Tracing m) => Text -> m Bool
storedFlag k = fromMaybe False <$> stored k

{- | The Feast of Hemlock Vale achievement detection. Hooked from the campaign's
runMessage (campaign dispatch runs for every message, BEFORE the scenario and
other entities, so defeated enemies etc. are still queryable).
'earnAchievement' self-gates on the achievements setting and on the campaign
id ("10"), so earns stay unconditional here.

Campaign-persistent trackers live in the campaign store (via 'SetGlobal'
messages, read back with 'stored'); the whole module is additionally gated to
achievement-eligible campaigns, matching every other campaign's detection
module. This list is printed for the campaign itself, so there is no Return-to
variant to inherit the hook.

"Complete the campaign" is 'CampaignStep EpilogueStep'. Every ending of Fate of
the Vale except the no-resolution one reaches it (they all call
@endOfScenarioThen EpilogueStep@); becoming the true feast calls 'gameOver'
directly and never does. Detections therefore key on the epilogue step rather
than on a resolution, which the Scenario wrapper clearQueues around anyway.
-}
module Arkham.Campaign.Campaigns.TheFeastOfHemlockVale.Achievements (
  runHemlockValeAchievements,
) where

import Arkham.Achievement
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Types (Field (CampaignMeta), campaignDifficulty)
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (
  Day (..),
  Resident (..),
  TheFeastOfHemlockValeMeta (..),
  Time (Night),
  getRelationshipLevel,
  initMeta,
  relationshipKey,
  pattern Omega,
  pattern Phi,
  pattern Psi,
  pattern Sigma,
 )
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
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
import Arkham.Helpers.Log (getRecordCount)
import Arkham.Helpers.Scenario (scenarioFieldMap)
import Arkham.Id
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher hiding (EncounterCardSource)
import Arkham.Message
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (ScenarioTokens, ScenarioVictoryDisplay))
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Trait qualified as Trait
import Arkham.UltimatumsAndBoons.Types
import Data.Aeson.Key qualified as Key

runHemlockValeAchievements
  :: (HasGame m, HasQueue Message m) => Message -> m ()
runHemlockValeAchievements msg = whenEligibleCampaign $ case msg of
  {- Endings. Every one records its own outcome, so the record IS the ending; the
  set is banked in the campaign store and reported to the API layer, which
  accumulates it across playthroughs for "Unshattered".

  Resolution 2 — the investigators giving themselves to the Vale — is also "High
  Dive" on its own. Both live in ONE alternative deliberately: case alternatives
  are first-match, so a separate `Record key` branch for High Dive would swallow
  that record and Unshattered would never see the ending (which is exactly what
  happened).

  Keyed on the records rather than the resolutions themselves, since the Scenario
  wrapper clearQueues twice while processing a resolution.
  -}
  Record key -> do
    when (key == toCampaignLogKey TheInvestigatorsSacrificedThemselvesForTheVale) do
      earn HighDive
    for_ (lookup key endingItems) \item -> do
      endings <- nub . (item :) <$> storedTexts endingsSeenKey
      setStore endingsSeenKey endings
      achievementProgress (TheFeastOfHemlockValeAchievement Unshattered) endings

    {- "Dancing Queen": share a dance with four different residents during The
    Second Evening. Each dance is its own campaign-log record, so the set of
    records IS the set of partners. -}
    when (key `elem` danceRecords) do
      partners <- nub . (tshow key :) <$> storedTexts dancePartnersKey
      setStore dancePartnersKey partners
      when (length partners >= 4) $ earn DancingQueen

  {- "Let's Do the Time Warp!": Lambs to the Slaughter's objective sends the
  prelude to Resolution 3, whose replay branch crosses out "the investigators
  believed" and starts the evening over — the time warp itself. The cross-out is
  pushed by the player's answer, i.e. after the resolution's own clearQueues, so
  it survives where the resolution message would not.
  -}
  CrossOutRecord key
    | key == toCampaignLogKey TheInvestigatorsBelieved ->
        whenScenarioIs preludeTheFinalEveningId $ earn LetsDoTheTimeWarp
  {- "Best Friends Forever!": reported the moment a resident reaches Relationship
  Level 6, not at the end. The printed achievement asks only that the level be
  reached, so a level later spent or lost still counts, and the checklist shows
  progress while the campaign is still running.

  The campaign dispatches before its own runner applies the change, so the new
  level is worked out here rather than read back.
  -}
  IncrementRecordCount key n -> reportBestFriend key . (+ n) =<< getRecordCount key
  RecordCount key n -> reportBestFriend key n
  {- "A Strong, Silent Type" bookkeeping: finish the campaign without VOLUNTARILY
  triggering a codex entry. Every trigger routes through
  'Arkham.Campaigns.TheFeastOfHemlockVale.Helpers.codex', which pushes this
  message, so one hook covers all of them.

  What counts as involuntary is a ruling, not something the engine marks:

    * Sigma and entry 17 are only ever pushed by acts and agendas (Desperate
      Search, On the Trail, Dawn of the Second Day, Desolation, Unsettling
      Silence) - no ability of the player's triggers them anywhere.
    * The Silent Heath's Omega, Psi and Phi come from the cave itself (Salt
      Chamber, Barn, Larval Tunnel, Crystal Nursery) rather than from a resident
      you chose to talk to.

  Everything else - the residents' parley actions, Dr. Marquez's reaction, the
  Bertie and Servant of Flame abilities - is the player choosing to open the
  codex, and disqualifies.
  -}
  ScenarioSpecific "codex" v -> do
    let (_ :: InvestigatorId, _ :: Source, entry :: Int) = toResult v
    sid <- selectOne TheScenario
    unless (isInvoluntaryCodex sid entry) $ setStore codexTriggeredKey True

  {- "Colour Outside the Lines" bookkeeping: the two skippable scenarios are only
  reachable by choosing to search at an evening prelude, so setting either up is
  proof it was not skipped. Latched for the whole playthrough.
  -}
  Setup -> do
    whenScenarioIs theTwistedHollowId $ setStore playedTwistedHollowKey True
    whenScenarioIs theLongestNightId $ setStore playedLongestNightKey True

  {- The earn, at the moment the second skip is locked in. Each evening resolves by
  pushing NextCampaignStep: either the optional scenario (searching) or a survey
  scenario (skipping it). Once the second evening has picked something OTHER than
  The Longest Night, neither optional scenario can be played this playthrough, so
  there is no reason to make the player wait for the epilogue to find out.

  The day alone does NOT identify that evening: the campaign flips the meta to
  Day2 as it steps INTO Dawn of the Second Day, and that prelude ends by picking
  the day's survey scenario, which is another explicit NextCampaignStep. Only the
  evening preludes set time = Night (each dawn resets it to Day), so the pair is
  what pins this to the second evening's choice.
  -}
  NextCampaignStep (Just continuation) -> do
    let nextStep = continuation.unwrap.normalize
    when (nextStep /= theLongestNightStep && nextStep /= theTwistedHollowStep) do
      meta <- hemlockValeMeta
      when (meta.day == Day2 && meta.time == Night) do
        playedHollow <- storedFlag playedTwistedHollowKey
        playedNight <- storedFlag playedLongestNightKey
        unless (playedHollow || playedNight) $ earn ColourOutsideTheLines

  {- "Here, Crabby Crabby!": the Limulus Hybrid flipping eight times in one game
  of The Lost Sister. Both of its sides push Flip at the enemy, so counting the
  message counts the flips.
  -}
  Flip _ _ (EnemyTarget eid) -> whenScenarioIs theLostSisterId do
    cardDef <- fieldMap Enemy.EnemyCard toCardDef eid
    when (cardDef `elem` limulusHybrids) do
      n <- storedInt limulusFlipsKey
      setStore limulusFlipsKey (n + 1)
      when (n + 1 >= 8) $ earn HereCrabbyCrabby

  {- "A Different Kind of Sting Ops" bookkeeping: the Brood Queen only ever
  reaches the table by being pulled out of the set-aside pile.
  -}
  CreateEnemy creation -> whenScenarioIs theSilentHeathId do
    let cardDef = toCardDef creation.card
    when (cardDef == Enemies.broodQueenDyingMother) $ setStore broodQueenSpawnedKey True

  {- "Bear Necessities" bookkeeping: the Ursine Hybrid has to go down to scenario
  card effects alone, so any damage from a player card disqualifies it.
  -}
  Damaged (EnemyTarget eid) da | da.amount > 0 -> whenScenarioIs theLongestNightId do
    cardDef <- fieldMap Enemy.EnemyCard toCardDef eid
    when (cardDef `elem` ursineHybrids && not (isScenarioSource da.source)) do
      setStore bearHurtByPlayerKey True

  {- Enemy defeats. ONE alternative for all of them: case alternatives are
  first-match, so a second `Defeated` branch would silently never run. The campaign
  sees Defeated before the enemy processes it, so the dying enemy is queryable.
  -}
  Defeated (EnemyTarget eid) _ _ _ -> do
    cardDef <- fieldMap Enemy.EnemyCard toCardDef eid

    -- "Bear Necessities": the bear with nothing but scenario effects on it.
    whenScenarioIs theLongestNightId $ when (cardDef `elem` ursineHybrids) do
      unlessM (storedFlag bearHurtByPlayerKey) $ earn BearNecessities

    -- "Settling the Score": the Thing in the Depths, in its own scenario.
    whenScenarioIs theThingInTheDepthsId do
      when (cardDef == Enemies.thingInTheDepths) $ earn SettlingTheScore

  {- "Oblivion Shmoblivion": win Fate of the Vale with every Cosmic Emissary in
  the victory display. 'EndOfGame' is what each resolution pushes after its own
  clearQueues, and the board is still intact when it dispatches (teardown happens
  at the EndOfScenario it queues), so the victory display can still be read. The
  no-resolution ending calls gameOver instead and never gets here, so this is also
  the "won it" signal.
  -}
  EndOfGame _ -> do
    whenScenarioIs fateOfTheValeId do
      inVictory <- allM inVictoryDisplay cosmicEmissaries
      when inVictory $ earn OblivionShmoblivion

    {- "Audrey III": finish a scenario engaged with a Poisonblossom carrying ten or
    more overgrowth. Any scenario counts, so this is not scoped to one.
    -}
    whenM (selectAny $ enemyIs Enemies.poisonblossom <> EnemyIsEngagedWith Anyone) do
      blossoms <- select $ enemyIs Enemies.poisonblossom <> EnemyIsEngagedWith Anyone
      overgrown <- filterM (fmap (>= 10) . fieldMap Enemy.EnemyTokens (countTokens Overgrowth)) blossoms
      when (notNull overgrown) $ earn AudreyIII

    -- "Hold on to your Potatoes!": finish Written in Rock holding both.
    whenScenarioIs writtenInRockId do
      hasResident <- selectAny $ AssetWithTrait Trait.Resident <> AssetControlledBy Anyone
      hasShard <- selectAny $ assetIs Assets.prismaticShardAlienMeteorite <> AssetControlledBy Anyone
      when (hasResident && hasShard) $ earn HoldOnToYourPotatoes

    {- "Dream Home Breakover": finish Hemlock House with ten rooms claimed. A
    claimed room lands in the victory display as its OTHER side, an enemy-location
    card, so counting plain locations finds none of them — the scenario's own
    end-of-game bonus counts EnemyLocationCardType for the same reason.
    -}
    whenScenarioIs hemlockHouseId do
      rooms <-
        scenarioFieldMap ScenarioVictoryDisplay (count ((== EnemyLocationCardType) . toCardType))
      when (rooms >= 10) $ earn DreamHomeBreakover

    -- "A Different Kind of Sting Ops": the Brood Queen never came out.
    whenScenarioIs theSilentHeathId do
      unlessM (storedFlag broodQueenSpawnedKey) $ earn ADifferentKindOfStingOps

    {- "Wait, There's No Shrouded Shrine?": survive The Twisted Hollow in Standalone
    Mode at Darkness Level 10 or more. Standalone is the mode with no campaign
    attached, which is exactly when the achievement setting still applies.
    -}
    whenScenarioIs theTwistedHollowId do
      darkness <- scenarioFieldMap ScenarioTokens (countTokens DarknessLevel)
      when (darkness >= 10) $ earn WaitTheresNoShroudedShrine
  -- End of campaign. See the module header for why this is the completion signal.
  CampaignStep EpilogueStep -> do
    g <- getGame
    let difficulty = campaignDifficulty . toAttrs <$> currentCampaign (gameMode g)

    -- "Aperitif": complete the campaign once, on any difficulty.
    earn Aperitif

    {- The relationship achievements. Levels live in the campaign log as record
    counts, so they survive the investigators themselves being killed by the
    ending that sacrifices them.
    -}
    for_ relationshipEarns \(resident, level, achievement) -> do
      current <- getRelationshipLevel resident
      when (current >= level) $ earn achievement

    {- "Captivating Scream": win as Patrice Hathaway. IncludeEliminated matters:
    Resolution 2 kills every investigator on the way to the epilogue, so a plain
    investigator matcher would find nobody.
    -}
    whenM (selectAny $ IncludeEliminated $ investigatorIs Investigators.patriceHathaway) do
      earn CaptivatingScream

    -- "Line in the Sand": win with at least 3 Ultimatums active.
    let ultimatums = length [u | Ultimatum u <- toList $ activeUltimatumsAndBoons (gameSettings g)]
    when (ultimatums >= 3) $ earn HemlockLineInTheSand

    {- "Life of the Party": every resident at Relationship Level 2 or higher. All
    eight count, not just the five the Best Friends checklist names.
    -}
    levels <- traverse getRelationshipLevel [minBound .. maxBound]
    when (all (>= 2) levels) $ earn LifeOfTheParty

    {- "Colour Outside the Lines" again, as a backstop: the earn above fires when
    the second skip is chosen, and this catches a campaign that reached the end
    without either scenario for any other reason.
    -}
    playedHollow <- storedFlag playedTwistedHollowKey
    playedNight <- storedFlag playedLongestNightKey
    unless (playedHollow || playedNight) $ earn ColourOutsideTheLines

    -- "A Strong, Silent Type": finished without voluntarily opening the codex.
    unlessM (storedFlag codexTriggeredKey) $ earn AStrongSilentType

    -- "Hemlock Expertise": win on Expert.
    when (difficulty == Just Expert) $ earn HemlockExpertise
  _ -> pure ()

earn :: (HasGame m, HasQueue Message m) => TheFeastOfHemlockValeAchievement -> m ()
earn = earnAchievement . TheFeastOfHemlockValeAchievement

{- | Gate the whole module to campaigns that can earn these achievements. Derived
from 'achievementCampaigns' so this cannot drift from 'earnAchievement''s own
campaign gate.
-}
whenEligibleCampaign :: HasGame m => m () -> m ()
whenEligibleCampaign body = do
  mCampaignId <- currentCampaignId
  let eligible = achievementCampaigns $ TheFeastOfHemlockValeAchievement Aperitif
  when (maybe False (`elem` eligible) mCampaignId) body

{- | Codex entries that are forced on the investigators rather than chosen. See
the note on the detection above for where the ruling comes from.
-}
isInvoluntaryCodex :: Maybe ScenarioId -> Int -> Bool
isInvoluntaryCodex sid entry =
  entry `elem` [Sigma, 17] || (sid == Just theSilentHeathId && entry `elem` [Omega, Psi, Phi])

preludeTheFinalEveningId, fateOfTheValeId, writtenInRockId, hemlockHouseId :: ScenarioId
preludeTheFinalEveningId = "10679b"
fateOfTheValeId = "10651"
writtenInRockId = "10501"
hemlockHouseId = "10523"

theSilentHeathId, theLostSisterId, theThingInTheDepthsId :: ScenarioId
theSilentHeathId = "10549"
theLostSisterId = "10569"
theThingInTheDepthsId = "10588"

theTwistedHollowId, theLongestNightId :: ScenarioId
theTwistedHollowId = "10605"
theLongestNightId = "10626"

-- | The two optional scenarios as campaign steps, for reading a chosen next step.
theTwistedHollowStep, theLongestNightStep :: CampaignStep
theTwistedHollowStep = ScenarioStep theTwistedHollowId
theLongestNightStep = ScenarioStep theLongestNightId

{- | The day/time meta, defaulting rather than throwing: the campaign builds it at
its prologue, so it is absent before then (and in the test harness).
-}
hemlockValeMeta :: HasGame m => m TheFeastOfHemlockValeMeta
hemlockValeMeta =
  selectOne TheCampaign
    >>= maybe (pure initMeta) (fieldMap CampaignMeta (toResultDefault initMeta))

{- | Every "shared a dance" record. Each names a different partner, so the set of
these recorded is the set of residents danced with.
-}
danceRecords :: [CampaignLogKey]
danceRecords =
  map
    toCampaignLogKey
    [ LeahAtwoodNotes LeahSharedADance
    , SimeonAtwoodNotes SimeonSharedADance
    , WilliamHemlockNotes WilliamSharedADance
    , RiverHawthorneNotes RiverSharedADance
    , GideonMizrahNotes GideonSharedADance
    , JudithParkNotes JudithSharedADance
    , TheoPetersNotes TheoSharedADance
    ]

-- | Both sides of the Limulus Hybrid; either flipping is a flip.
limulusHybrids :: [CardDef]
limulusHybrids = [Enemies.limulusHybridInTheLight, Enemies.limulusHybridInTheDark]

-- | Both sides of the Ursine Hybrid.
ursineHybrids :: [CardDef]
ursineHybrids =
  [Enemies.ursineHybridGlowingAbomination, Enemies.ursineHybridStarvingAbomination]

{- | Whether damage came from a SCENARIO card effect, which is what "Bear
Necessities" allows. Written as an allow-list rather than a player-card deny-list:
a plain fight action is sourced to the INVESTIGATOR, not to a weapon, so checking
only for assets and events let an ordinary attack through.

Unwraps the ability/payment/proxy wrappers first, the same way the built-in
'Source' accessors do.
-}
isScenarioSource :: Source -> Bool
isScenarioSource = go
 where
  go = \case
    {- A BASIC ability (index 100 - fight, evade, investigate) is the investigator
    taking an action, even though the ability lives on the card being acted upon:
    a plain attack is sourced to the ENEMY's own fight ability. Checked before the
    wrappers are stripped, since stripping is what hid it. -}
    AbilitySource _ 100 -> False
    UseAbilitySource _ _ 100 -> False
    AbilitySource s _ -> go s
    UseAbilitySource _ s _ -> go s
    IndexedSource _ s -> go s
    ProxySource s _ -> go s
    PaymentSource s -> go s
    BothSource a b -> go a && go b
    ActSource _ -> True
    ActDeckSource -> True
    AgendaSource _ -> True
    AgendaDeckSource -> True
    LocationSource _ -> True
    TreacherySource _ -> True
    StorySource _ -> True
    EnemySource _ -> True
    EnemyAttackSource _ -> True
    EnemyDefeatSource _ -> True
    ScenarioSource -> True
    ChaosTokenSource _ -> True
    ChaosTokenEffectSource _ -> True
    EncounterCardSource _ -> True
    GameSource -> True
    _ -> False

{- | Report a resident's checklist item once their Relationship Level reaches 6.
Only the five residents "Best Friends Forever!" names have an item; the others
have their own achievements instead.
-}
reportBestFriend
  :: (HasGame m, HasQueue Message m) => CampaignLogKey -> Int -> m ()
reportBestFriend key level = when (level >= 6) do
  for_ (find ((== key) . relationshipKey . fst) bestFriends) \(_, item) -> do
    reached <- nub . (item :) <$> storedTexts bestFriendsKey
    setStore bestFriendsKey reached
    achievementProgress (TheFeastOfHemlockValeAchievement BestFriendsForever) reached

{- | The five residents "Best Friends Forever!" wants at Relationship Level 6,
paired with their 'achievementChecklist' item keys.
-}
bestFriends :: [(Resident, Text)]
bestFriends =
  [ (LeahAtwood, "LeahAtwood")
  , (SimeonAtwood, "SimeonAtwood")
  , (RiverHawthorne, "RiverHawthorne")
  , (GideonMizrah, "GideonMizrah")
  , (WilliamHemlock, "WilliamHemlock")
  ]

-- | The four Cosmic Emissaries "Oblivion Shmoblivion" wants in the victory display.
cosmicEmissaries :: [CardDef]
cosmicEmissaries =
  [ Enemies.cosmicEmissaryTheAbyss
  , Enemies.cosmicEmissaryTheMiasma
  , Enemies.cosmicEmissaryTheBrilliance
  , Enemies.cosmicEmissaryThePhantasm
  ]

inVictoryDisplay :: HasGame m => CardDef -> m Bool
inVictoryDisplay def = selectAny $ VictoryDisplayCardMatch $ basic $ cardIs def

{- | Each ending of the campaign paired with its 'achievementChecklist' item key.
The record a Fate of the Vale resolution writes is that ending.
-}
endingItems :: [(CampaignLogKey, Text)]
endingItems =
  [ (toCampaignLogKey DrMarquezSacrificedHerselfForTheVale, "MarquezSacrificedHerself")
  ,
    ( toCampaignLogKey TheInvestigatorsSacrificedThemselvesForTheVale
    , "TheInvestigatorsSacrificedThemselves"
    )
  , (toCampaignLogKey TheValeWasSaved, "TheValeWasSaved")
  , (toCampaignLogKey TheValeBurned, "TheValeBurned")
  , (toCampaignLogKey TheInvestigatorsBarelySurvivedTheFeastOfHemlockVale, "BarelySurvivedTheFeast")
  , (toCampaignLogKey TheInvestigatorsBecameTheTrueFeastOfHemlockVale, "BecameTheTrueFeast")
  ]

whenScenarioIs :: HasGame m => ScenarioId -> m () -> m ()
whenScenarioIs sid body = do
  mSid <- selectOne TheScenario
  when (mSid == Just sid) body

-- Campaign store plumbing. Writes go through the queue ('SetGlobal' is handled
-- by the campaign runner); reads see all previously processed writes.

endingsSeenKey, dancePartnersKey, limulusFlipsKey, bestFriendsKey :: Text
bestFriendsKey = "hemlockAchBestFriends"
endingsSeenKey = "hemlockAchEndingsSeen"
dancePartnersKey = "hemlockAchDancePartners"
limulusFlipsKey = "hemlockAchLimulusFlips"

broodQueenSpawnedKey, bearHurtByPlayerKey :: Text
broodQueenSpawnedKey = "hemlockAchBroodQueenSpawned"
bearHurtByPlayerKey = "hemlockAchBearHurtByPlayer"

codexTriggeredKey :: Text
codexTriggeredKey = "hemlockAchCodexTriggered"

playedTwistedHollowKey, playedLongestNightKey :: Text
playedTwistedHollowKey = "hemlockAchPlayedTwistedHollow"
playedLongestNightKey = "hemlockAchPlayedLongestNight"

-- Priority so the write is applied before the rest of the triggering message's
-- cascade — some cascades clearQueue, which would otherwise drop it.
setStore :: (HasQueue Message m, ToJSON a) => Text -> a -> m ()
setStore k v = push $ Priority $ SetGlobal CampaignTarget (Key.fromText k) (toJSON v)

storedTexts :: (HasCallStack, HasGame m) => Text -> m [Text]
storedTexts k = fromMaybe [] <$> stored k

storedInt :: (HasCallStack, HasGame m) => Text -> m Int
storedInt k = fromMaybe 0 <$> stored k

storedFlag :: (HasCallStack, HasGame m) => Text -> m Bool
storedFlag k = fromMaybe False <$> stored k

{- | Each resident whose Relationship Level is an achievement in its own right,
with the level the printed achievement asks for.
-}
relationshipEarns :: [(Resident, Int, TheFeastOfHemlockValeAchievement)]
relationshipEarns =
  [ (MotherRachel, 3, KnowYourPlace)
  , (JudithPark, 7, HeartOfSteel)
  , (TheoPeters, 7, HoldingOutForAHimbo)
  ]

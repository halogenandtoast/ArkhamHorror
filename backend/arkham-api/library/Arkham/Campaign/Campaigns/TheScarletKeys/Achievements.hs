{- | The Scarlet Keys achievement detection. Hooked from the campaign's
runMessage (campaign dispatch runs for every message, BEFORE the scenario and
other entities, so defeated enemies etc. are still queryable).
'earnAchievement' self-gates on the achievements setting and on the campaign
id ("09"), so earns stay unconditional here.

Campaign-persistent trackers live in the campaign store (via 'SetGlobal'
messages, read back with 'stored'); the whole module is additionally gated to
achievement-eligible campaigns, matching every other campaign's detection
module. Like The Drowned City and The Innsmouth Conspiracy — and unlike the
Return-to lists — this list is printed for the campaign itself, so there is no
separate variant to inherit the hook.

"Win the campaign" is exactly 'CampaignStep EpilogueStep': Congress of the
Keys is the last scenario, and only its Resolution 1 (The Outsiders Were
Stopped) reaches the epilogue — Resolution 2 records The Cell Was Hollowed and
calls 'gameOver' instead. Detections therefore key on the epilogue step rather
than on the resolution, which the Scenario wrapper clearQueues around anyway.
-}
module Arkham.Campaign.Campaigns.TheScarletKeys.Achievements (
  runScarletKeysAchievements,
) where

import Arkham.Achievement
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types qualified as Asset
import Arkham.Campaign.Types (Field (CampaignChaosBag), campaignDifficulty)
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Types (Field (ScarletKeyCardCode, ScarletKeyPlacement))
import Arkham.Campaigns.TheScarletKeys.Meta
import Arkham.Card
import Arkham.ChaosToken
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
import Arkham.Helpers.Log (getRecordCount, scenarioCount)
import Arkham.Id
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types qualified as Location
import Arkham.Matcher hiding (AssetDefeated, PlaceUnderneath)
import Arkham.Message
import Arkham.Movement (Destination (ToLocation))
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.ScenarioLogKey (ScenarioCountKey (CiviliansSlain))
import Arkham.Source
import Arkham.Story.Cards qualified as Stories
import Arkham.Story.Types (Field (StoryClues))
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Tracing
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.UltimatumsAndBoons.Types
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (parseMaybe)

runScarletKeysAchievements
  :: (HasGame m, HasQueue Message m, Tracing m) => Message -> m ()
runScarletKeysAchievements msg = whenEligibleCampaign $ case msg of
  {- "Trust Nobody" / "Trust Everybody" bookkeeping. The campaign's only routine
  bag change is 'swapTokens', which removes one face and adds the other, so a
  removal of the watched face is exactly "traded that trust away". The guard
  matters because 'swapTokens' pushes the removal unconditionally: once a face is
  down to zero the message keeps arriving as a no-op, which is not a removal.
  The campaign dispatches before its own runner applies the change, so the bag
  read here is the pre-removal one.
  -}
  RemoveChaosToken face | face `elem` watchedFaces -> do
    whenM (bagContains face) $ setStore (removedKey face) True

  {- "Clued In" bookkeeping. A treachery takes a clue off you in two ways, and the
  engine attributes them differently.

  DROPPING is sourced: Heavy Rain and False Lead both push
  'InvestigatorPlaceCluesOnLocation' carrying the treachery, so any
  treachery-sourced drop disqualifies — no card list needed.

  SPENDING is not sourced at all: 'InvestigatorSpendClues' names only the
  investigator, and Riddles and Rain's own Elder Thing token spends a clue too, so
  the bare message would false-positive on a chaos token. Pinch in Reality is the
  scenario's only clue-spending treachery and is in play for exactly as long as its
  own revelation is resolving, so a spend while it is on the table is its spend.
  -}
  InvestigatorPlaceCluesOnLocation _ source n | n > 0 -> treacheryTookClues source
  InvestigatorPlaceAllCluesOnLocation iid source ->
    whenM ((> 0) <$> field InvestigatorClues iid) $ treacheryTookClues source
  InvestigatorSpendClues _ n | n > 0 -> whenScenarioIs riddlesAndRainId do
    whenM (selectAny $ mapOneOf treacheryIs clueSpendingTreacheries) do
      setStore cluesLostToTreacheryKey True

  {- '"I\'m Just Here for the Local Cuisine"'. Each of the five cities has its own
  moment rather than being satisfied by arriving, so they are collected one at a
  time into a campaign-store set. See 'cuisineCities' for what each one is.

  -}
  -- Kuala Lumpur: buying a round at the Selangor Club, its parley ability.
  UseThisAbility _ source 1 | Just lid <- source.location -> do
    cardDef <- fieldMap Location.LocationCard toCardDef lid
    when (cardDef == Locations.selangorClub) $ sampled KualaLumpur

  {- Havana: arriving at Cafe Luna in Dancing Mad, either side of it.

  Keyed on the After of the movement, NOT on 'EnterLocation': the investigator
  runner nests EnterLocation inside a @Simultaneously $ Run [...]@ block, so it
  never dispatches as a top-level message and the campaign never sees it. The
  After does, and it covers the scenario's own @startAt@ placement too (that
  routes through the same movement path with @means = Place@).
  -}
  After (MoveTo movement) | ToLocation lid <- movement.destination -> do
    cardDef <- fieldMap Location.LocationCard toCardDef lid
    when (cardDef `elem` cafeLuna) $ sampled Havana

  -- Buenos Aires: Sanguine Shadows' intro, i.e. simply turning up there.
  PreScenarioSetup -> whenScenarioIs sanguineShadowsId $ sampled BuenosAires
  {- Marrakesh: Dead Heat's intro 2 or 3, the rooftop cafe where Latif lays out a
  meal. Intro 4 is the arrive-too-late opening — the cafe is boarded up and there is
  nothing to eat — so it deliberately does not count.
  -}
  DoStep n PreScenarioSetup
    | n `elem` [2, 3] ->
        whenScenarioIs deadHeatId $ sampled Marrakesh
  {- "With Your Powers Combined…": shift five keys in one turn.

  Keyed on the window 'shiftKey' raises, because that is the ONE point every shift
  passes through. The two routes into it look nothing alike: cards and enemies call
  'Arkham.Campaigns.TheScarletKeys.Helpers.shift', which pushes a
  @CampaignSpecific "shift[<code>]"@ the key then handles, while a key's own fast
  ability runs that same message on itself with 'liftRunMessage' — so it is never
  queued and the campaign never sees it. Both end in @shiftKey@.

  Only the After window is counted; 'shiftKey' brackets the shift with a When one
  too. Keys are tallied by id so shifting the same key repeatedly is not five.
  -}
  CheckWindows ws | Just kid <- firstShiftedKey ws -> do
    shifted <- nub . (kid :) <$> storedTexts shiftedKeysKey
    setStore shiftedKeysKey shifted
    when (length shifted >= 5) $ earn ScarletWithYourPowersCombined
  -- "in a single turn" — cleared at both ends, per the per-turn counter convention.
  BeginTurn _ -> setStore shiftedKeysKey ([] :: [CardCode])
  EndTurn _ -> setStore shiftedKeysKey ([] :: [CardCode])
  {- "Gift of Gab": Taylor orders you to "talk" three times. Special Delivery
  (interlude 37) is repeatable — it fires on travelling to Lagos or Tokyo — and
  these are exactly its two branches that hand over intel, i.e. the two whose text
  is Taylor's "Talk." The other two branches (2 and 4) are the hand-BACK, where she
  says nothing of the kind.
  -}
  DoStep n (CampaignStep (InterludeStep 37 _)) | n `elem` [1, 3] -> do
    talks <- storedInt taylorTalksKey
    setStore taylorTalksKey (talks + 1)
    when (talks + 1 >= 3) $ earn GiftOfGab
    -- Tokyo's cuisine moment is Special Delivery 1, which IS the in-Tokyo branch
    -- (branch 3 is the same handover in Lagos).
    when (n == 1) $ sampled Tokyo

  {- "Take That, Ghulat" / "What's in a Name?" bookkeeping and earns live on Dead
  Heat. Civilians are only ever slain through the scenario's own 'slayCivilian',
  which bumps the CiviliansSlain scenario count, so zero at the end IS none slain.
  -}
  EndOfGame _ -> do
    whenScenarioIs riddlesAndRainId do
      unlessM (storedFlag cluesLostToTreacheryKey) $ earn CluedIn
    whenScenarioIs deadHeatId do
      slain <- scenarioCount CiviliansSlain
      when (slain == 0) $ earn TakeThatGhulat
    whenScenarioIs shadesOfSufferingId do
      -- "Under My Umbrella": Tzu San Niang never ate a Geist.
      unlessM (storedFlag geistDevouredKey) $ earn UnderMyUmbrella
    whenScenarioIs onThinIceId do
      forms <- storedTexts chimeraFormsKey
      when (length forms >= length voidChimeraForms) $ earn MoreLikeDestroyedChimera

  {- "Under My Umbrella" bookkeeping. Restless Dead's B side is the only devouring:
  if Tzu San Niang is still in the shadows when agenda 2 advances she surfaces,
  removes every Geist at her location from the game and tucks them under herself.
  The tuck is the observable half.
  -}
  PlaceUnderneath (EnemyTarget eid) cards | notNull cards ->
    whenScenarioIs shadesOfSufferingId do
      cardDef <- fieldMap Enemy.EnemyCard toCardDef eid
      when (cardDef == Enemies.tzuSanNiangTheLadyWithTheRedParasol) do
        setStore geistDevouredKey True

  {- "More Like \"Destroyed\" Chimera": all five forms in one game. The campaign sees
  Defeated before the enemy does, so the dying form is still queryable.
  -}
  Defeated (EnemyTarget eid) _ _ _ -> do
    cardDef <- fieldMap Enemy.EnemyCard toCardDef eid
    whenScenarioIs onThinIceId $ when (cardDef `elem` voidChimeraForms) do
      forms <- nub . (toCardCode cardDef :) <$> storedTexts' chimeraFormsKey
      setStore chimeraFormsKey forms
      when (length forms >= length voidChimeraForms) $ earn MoreLikeDestroyedChimera

    {- "Porque No Los Dos?": both copies of Desi defeated by the same damage
    assignment. One effect killing both queues BOTH Defeated messages before either
    is dispatched, so the other copy's Defeated is still sitting in the queue when
    this one arrives; a second, separate blow is queued only after this one has
    already been consumed. -}
    whenScenarioIs dancingMadId $ when (cardDef `elem` desiCopies) do
      let otherCopies = filter (/= cardDef) desiCopies
      others <- select $ mapOneOf enemyIs otherCopies
      {- One effect damaging both queues a DealDamage per copy up front, so the
      other copy's damage is still pending when this one's defeat resolves (its own
      CheckDefeated is popped first). A second, separate blow is only queued after
      this defeat has already been consumed, so nothing is pending then. The other
      copy's own Defeated is matched too, for effects that defeat outright rather
      than through damage. -}
      queuedTwin <- findFromQueue \case
        DealDamage (EnemyTarget other) _ -> other `elem` others
        Defeated (EnemyTarget other) _ _ _ -> other `elem` others
        _ -> False
      when (isJust queuedTwin) $ earn PorqueNoLosDos

  {- "Lost and Found": take control of the Twisted Antiprism with Clues Unveiled
  still bare. Search for the Talisman hands the key over with
  'createScarletKeyAt_ … AttachedToInvestigator'; the campaign dispatches before
  the story is touched, so the clue count read here is the one at the handover.
  -}
  CreateScarletKeyAt card (AttachedToInvestigator _) | toCardDef card == Keys.theTwistedAntiprism ->
    whenScenarioIs dealingsInTheDarkId do
      unveiled <- selectOne (storyIs Stories.theUnveiling) >>= maybe (pure 0) (field StoryClues)
      when (unveiled == 0) $ earn LostAndFound

  {- "I Like Tower Defense Games" bookkeeping: a Key Locus is destroyed by being
  defeated (its own forced ability then removes it from game).
  -}
  AssetDefeated _ aid -> whenScenarioIs dogsOfWarId do
    cardDef <- fieldMap Asset.AssetCard toCardDef aid
    when (cardDef `elem` keyLocuses) $ setStore keyLocusDestroyedKey True
  -- The earn: Rabbits who Run (v. I) advancing IS having defended the Knight.
  AdvanceAct aid _ _
    | unActId aid == toCardCode Acts.rabbitsWhoRunV1 ->
        unlessM (storedFlag keyLocusDestroyedKey) $ earn ILikeTowerDefenseGames
  {- "Play With Your Food": steal The Light of Pharos off the Claret Knight or the
  Beast while they are on their last point of health. Stealing it is the key being
  re-placed onto an investigator (which is also v. II's objective); the campaign
  runs before the placement, so the key still names the enemy it is coming off.
  -}
  PlaceScarletKey kid (AttachedToInvestigator _) -> whenScenarioIs dogsOfWarId do
    cCode <- field ScarletKeyCardCode kid
    when (cCode == toCardCode Keys.theLightOfPharos) do
      placement <- field ScarletKeyPlacement kid
      case placement of
        AttachedToEnemy eid -> do
          cardDef <- fieldMap Enemy.EnemyCard toCardDef eid
          when (cardDef `elem` pharosBearers) do
            remaining <- field Enemy.EnemyRemainingHealth eid
            when (remaining == Just 1) $ earn PlayWithYourFood
        _ -> pure ()

  {- "Who Watches the Watcher?": the secret final act is Seeing Red, which is set
  aside and only reaches the table when In the Searchlight's last step installs it
  as the current agenda deck. It is printed as both an agenda and an act.
  -}
  SetCurrentAgendaDeck _ cards
    | any ((== toCardCode Agendas.seeingRed) . toCardCode) cards ->
        whenScenarioIs sanguineShadowsId $ earn WhoWatchesTheWatcher
  {- "Key to My Heart": collect each of the eleven keys at some point in the
  campaign. A key is collected when it comes to rest with an investigator, which
  the campaign records through 'setBearer' — every route (a scenario's
  'chooseBearer', an interlude handing one over, taking one off a Coterie member)
  ends in this message. Progress is reported as each one arrives rather than at the
  end: the printed achievement asks only that each be collected "throughout the
  campaign", so a key later stolen back still counts, and the API layer accumulates
  the checklist across playthroughs.
  -}
  CampaignSpecific "setBearer" v
    | Just (cCode, KeyWithInvestigator _) <- parseMaybe (parseJSON @(CardCode, KeyStatus)) v ->
        for_ (lookup cCode keyChecklist) \item -> do
          collected <- nub . (item :) <$> storedTexts collectedKeysKey
          setStore collectedKeysKey collected
          achievementProgress (TheScarletKeysAchievement KeyToMyHeart) collected

  -- "What's in a Name?": Dead Heat's Resolution 3, the only writer of this record,
  -- is telling Amaranth who she really is.
  Record key | key == toCampaignLogKey AmaranthHasLeftTheCoterie -> earn WhatsInAName
  -- Per-game flags reset as their scenario is set up, so a revisited scenario
  -- cannot inherit a previous game's tally.
  {- "All Hollow": earned on UNLOCKING Without a Trace, not on playing it. The
  Bermuda Triangle is only ever unlocked by Romulus and Remus (interlude 51, when
  the cell is off mission), which is the "learn about a place where Outsiders
  dwell" half; the unlock is what makes the scenario reachable at all.
  -}
  CampaignSpecific "unlock" v | toResult v == BermudaTriangle -> earn AllHollow
  -- Per-game flags reset as their scenario is set up, so a revisited scenario
  -- cannot inherit a previous game's tally.
  Setup -> whenScenarioIs riddlesAndRainId $ setStore cluesLostToTreacheryKey False
  -- Records that ARE the achievement, keyed on the write rather than re-derived
  -- later: the two Red Coterie outcomes are only reachable from Congress of the
  -- Keys' trial, and the badge is written by the epilogue itself — i.e. after
  -- the campaign has already dispatched the epilogue step.
  Record key -> for_ (lookup key recordEarns) earn
  -- End of campaign, i.e. the win. See the module header.
  CampaignStep EpilogueStep -> do
    g <- getGame
    let difficulty = campaignDifficulty . toAttrs <$> currentCampaign (gameMode g)

    -- "Speed Demon": win with 17 or fewer time passed.
    time <- getRecordCount Time
    when (time <= 17) $ earn SpeedDemon

    -- "Trust Nobody" / "Trust Everybody": win with four of the face in the bag
    -- and never having removed one. 'swapTokens' caps each face at four, so
    -- "four" and "at least four" are the same condition.
    for_ watchedFaces \face -> do
      tokens <- bagCount face
      removed <- storedFlag (removedKey face)
      when (tokens >= 4 && not removed) $ earn (trustAchievement face)

    -- "Line in the Sand": win with at least 3 Ultimatums active.
    let ultimatums = length [u | Ultimatum u <- toList $ activeUltimatumsAndBoons (gameSettings g)]
    when (ultimatums >= 3) $ earn ScarletLineInTheSand

    -- "Global Expertise": win on Expert.
    when (difficulty == Just Expert) $ earn GlobalExpertise
  _ -> pure ()

earn :: (HasGame m, HasQueue Message m) => TheScarletKeysAchievement -> m ()
earn = earnAchievement . TheScarletKeysAchievement

{- | Gate the whole module (including store writes) to campaigns that can earn
these achievements. Derived from 'achievementCampaigns' so this cannot drift
from 'earnAchievement''s own campaign gate.
-}
whenEligibleCampaign :: HasGame m => m () -> m ()
whenEligibleCampaign body = do
  mCampaignId <- currentCampaignId
  let eligible = achievementCampaigns $ TheScarletKeysAchievement SpeedDemon
  when (maybe False (`elem` eligible) mCampaignId) body

whenScenarioIs :: (HasGame m, Tracing m) => ScenarioId -> m () -> m ()
whenScenarioIs sid body = do
  mSid <- selectOne TheScenario
  when (mSid == Just sid) body

-- | Riddles and Rain, the prologue scenario "Clued In" is scoped to.
riddlesAndRainId :: ScenarioId
riddlesAndRainId = "09501"

sanguineShadowsId, deadHeatId, dancingMadId :: ScenarioId
sanguineShadowsId = "09545"
deadHeatId = "09520"
dancingMadId = "09591"

dealingsInTheDarkId, onThinIceId, dogsOfWarId, shadesOfSufferingId :: ScenarioId
dealingsInTheDarkId = "09566"
onThinIceId = "09609"
dogsOfWarId = "09635"
shadesOfSufferingId = "09660"

{- | Each Scarlet Key paired with its 'achievementChecklist' item key, in printed
order. The Wellspring of Fortune comes from the Fortune and Folly side story, the
Mirroring Blade / Bale Engine / Ruinous Chime from Globetrotting; the rest are one
per scenario.
-}
keyChecklist :: [(CardCode, Text)]
keyChecklist =
  map
    (first toCardCode)
    [ (Keys.theEyeOfRavens, "TheEyeOfRavens")
    , (Keys.theLastBlossom, "TheLastBlossom")
    , (Keys.theLightOfPharos, "TheLightOfPharos")
    , (Keys.theSableGlass, "TheSableGlass")
    , (Keys.theWeepingLady, "TheWeepingLady")
    , (Keys.theTwistedAntiprism, "TheTwistedAntiprism")
    , (Keys.theShadeReaper, "TheShadeReaper")
    , (Keys.theMirroringBlade, "TheMirroringBlade")
    , (Keys.theBaleEngine, "TheBaleEngine")
    , (Keys.theRuinousChime, "TheRuinousChime")
    , (Keys.theWellspringOfFortune, "TheWellspringOfFortune")
    ]

-- | The five forms "More Like \"Destroyed\" Chimera" wants defeated in one game.
voidChimeraForms :: [CardDef]
voidChimeraForms =
  [ Enemies.voidChimeraTrueForm
  , Enemies.voidChimeraFellbeak
  , Enemies.voidChimeraEarsplitter
  , Enemies.voidChimeraGorefeaster
  , Enemies.voidChimeraFellhound
  ]

{- | Dancing Mad's two copies of Desi, both of which "Porque No Los Dos?" wants down
to the same blow.
-}
desiCopies :: [CardDef]
desiCopies = [Enemies.desiderioDelgadoAlvarez106, Enemies.desiderioDelgadoAlvarez107]

-- | The Key Locuses "I Like Tower Defense Games" wants left standing.
keyLocuses :: [CardDef]
keyLocuses = [Assets.keyLocusLastBastion, Assets.keyLocusDefensiveBarrier]

{- | The Coterie members who can be carrying The Light of Pharos when it is stolen:
the Claret Knight in Dogs of War v. II, the Beast in v. III.

These are the IN-SCENARIO printings that Dogs of War's setup actually spawns and
hangs the key on — not the "…Holds You in Contempt" / "…Leaving a Trail of
Destruction" versions the campaign log tracks between scenarios.
-}
pharosBearers :: [CardDef]
pharosBearers =
  [ Enemies.theClaretKnightCoterieKingpin
  , Enemies.theBeastInACowlOfCrimsonWolfInSheepsClothing
  ]

{- | Both sides of Cafe Luna, Havana's cuisine moment. Dancing Mad flips between
them, and entering either is entering the cafe.
-}
cafeLuna :: [CardDef]
cafeLuna = [Locations.cafeLunaCoterieHaunt, Locations.cafeLunaBastionOfRemembrance]

{- | The five cities the local-cuisine achievement wants a meal, a bar or a cafe
from. Each has its own moment rather than being satisfied by arriving:

  * Marrakesh — Dead Heat intro 2 or 3, the rooftop cafe (intro 4 does not count:
    you arrive too late and the cafe is boarded up).
  * Havana — entering Cafe Luna in Dancing Mad.
  * Buenos Aires — Sanguine Shadows' intro.
  * Tokyo — Special Delivery 1, Taylor's cafe handover.
  * Kuala Lumpur — using the Selangor Club's parley ability in Shades of Suffering.
-}
cuisineCities :: [MapLocationId]
cuisineCities = [Marrakesh, Havana, BuenosAires, Tokyo, KualaLumpur]

{- | Record a city's cuisine moment, earning once all five are collected. Cities
are stored by name so the set survives across scenarios and interludes.
-}
sampled :: (HasGame m, HasQueue Message m, Tracing m) => MapLocationId -> m ()
sampled city = do
  cities <- nub . (tshow city :) <$> storedTexts cuisineKey
  setStore cuisineKey cities
  when (all ((`elem` cities) . tshow) cuisineCities) $ earn ImJustHereForTheLocalCuisine

{- | The id of the key being shifted, out of the After half of the window
'Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted.shiftKey' raises around every
shift. The payload is the 'ScarletKeyId'.
-}
firstShiftedKey :: [Window] -> Maybe Text
firstShiftedKey ws =
  listToMaybe
    [ key
    | Window {windowTiming = Timing.After, windowType = Window.CampaignEvent "shiftKey" _ payload} <- ws
    , Just key <- [parseMaybe (parseJSON @Text) payload]
    ]

{- | Treacheries that make you SPEND a clue rather than drop one. Spending carries
no source (see the "Clued In" note above), so it can only be attributed to a
treachery that is on the table at the time.

These two are the WHOLE set in the game, not just this scenario's: they are the
only treachery implementations that reach 'spendClues' at all (Calling Card only
applies @CannotSpendClues@). Both happen to be in Riddles and Rain's pool — Pinch
in Reality from Strange Happenings, Hunting Shadow from the two Midnight Masks
cards the scenario gathers by name. Keep this list exhaustive rather than
scenario-scoped: a card missing from it fails silently, by handing out the
achievement.
-}
clueSpendingTreacheries :: [CardDef]
clueSpendingTreacheries = [Treacheries.pinchInReality, Treacheries.huntingShadow]

{- | Disqualify "Clued In" when the clues that just left an investigator were taken
by a treachery. Scoped to Riddles and Rain here rather than at both call sites.
-}
treacheryTookClues :: (HasGame m, HasQueue Message m, Tracing m) => Source -> m ()
treacheryTookClues source = whenScenarioIs riddlesAndRainId do
  when (isJust source.treachery) $ setStore cluesLostToTreacheryKey True

{- | Campaign-log records whose being written is itself the achievement.

"Red Looks Good on Me" and "Bloody Red Revolution" are the two Coterie-siding
outcomes of Congress of the Keys' trial (@DoStep 4@ / @DoStep 3
PreScenarioSetup@), offered only when the vote goes the cell's way and the
scenario's own @canJoin@ / @canOverthrow@ conditions hold. "Here is Your Badge"
is the epilogue's verdict when Foundation trust is at least the cell's deception.
-}
recordEarns :: [(CampaignLogKey, TheScarletKeysAchievement)]
recordEarns =
  [ (toCampaignLogKey TheCellJoinedTheRedCoterie, RedLooksGoodOnMe)
  , (toCampaignLogKey TheCellOverthrewTheRedCoterie, BloodyRedRevolution)
  , (toCampaignLogKey TheCellWasGivenAPermanentPosition, HereIsYourBadge)
  ]

-- | The two faces the campaign trades between, one per "trust" achievement.
watchedFaces :: [ChaosTokenFace]
watchedFaces = [ElderThing, Tablet]

-- | Elder Thing is the Coterie's mark, Tablet the Foundation's.
trustAchievement :: ChaosTokenFace -> TheScarletKeysAchievement
trustAchievement = \case
  Tablet -> TrustEverybody
  _ -> TrustNobody

bagCount :: (HasGame m, Tracing m) => ChaosTokenFace -> m Int
bagCount face =
  selectOne TheCampaign >>= maybe (pure 0) (fieldMap CampaignChaosBag (count (== face)))

bagContains :: (HasGame m, Tracing m) => ChaosTokenFace -> m Bool
bagContains face = (> 0) <$> bagCount face

-- Campaign store plumbing. Writes go through the queue ('SetGlobal' is handled
-- by the campaign runner); reads see all previously processed writes.

cluesLostToTreacheryKey, shiftedKeysKey, taylorTalksKey, cuisineKey :: Text
geistDevouredKey, chimeraFormsKey, keyLocusDestroyedKey, collectedKeysKey :: Text
collectedKeysKey = "tskAchCollectedKeys"
geistDevouredKey = "tskAchGeistDevoured"
chimeraFormsKey = "tskAchChimeraForms"
keyLocusDestroyedKey = "tskAchKeyLocusDestroyed"
cluesLostToTreacheryKey = "tskAchCluesLostToTreachery"
shiftedKeysKey = "tskAchShiftedKeys"
taylorTalksKey = "tskAchTaylorTalks"
cuisineKey = "tskAchCuisine"

removedKey :: ChaosTokenFace -> Text
removedKey face = "tskAchRemoved[" <> tshow face <> "]"

-- Priority so the write is applied before the rest of the triggering message's
-- cascade — some cascades clearQueue, which would otherwise drop a plainly
-- pushed store write.
setStore :: (HasQueue Message m, ToJSON a) => Text -> a -> m ()
setStore k v = push $ Priority $ SetGlobal CampaignTarget (Key.fromText k) (toJSON v)

storedFlag :: (HasCallStack, HasGame m, Tracing m) => Text -> m Bool
storedFlag k = fromMaybe False <$> stored k

storedInt :: (HasCallStack, HasGame m, Tracing m) => Text -> m Int
storedInt k = fromMaybe 0 <$> stored k

storedTexts :: (HasCallStack, HasGame m, Tracing m) => Text -> m [Text]
storedTexts k = fromMaybe [] <$> stored k

storedTexts' :: (HasCallStack, HasGame m, Tracing m) => Text -> m [CardCode]
storedTexts' k = fromMaybe [] <$> stored k

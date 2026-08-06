{- | The Drowned City achievement detection. Hooked from the campaign's
runMessage (campaign dispatch runs for every message, BEFORE the scenario and
other entities, so defeated enemies etc. are still queryable).
'earnAchievement' self-gates on the achievements setting and on the campaign
id ("11"), so earns stay unconditional here.

Campaign-persistent trackers live in the campaign store (via 'SetGlobal'
messages, read back with 'stored'); the whole module is additionally gated to
achievement-eligible campaigns, matching every other campaign's detection
module. Unlike the Return-to lists, this one is printed for the campaign
itself, so there is no separate variant to inherit the hook.

Scenario-end detections key on 'EndOfGame', NOT on 'ScenarioResolution': the
Scenario wrapper clearQueues twice while processing a resolution, wiping even
Priority pushes made during that dispatch. 'EndOfGame' is pushed by the
resolution body afterwards, and the board is still fully intact when it
dispatches (teardown happens at the 'EndOfScenario' it queues), so location
reveal/flood state and controlled Artifacts can all still be read.

Achievements that require the scenario to be BEATEN key on the final act's
'AdvanceAct' instead. "Still has an uneliminated investigator" is NOT a usable
stand-in for that: Court of the Ancients and The Drowned Quarter are beaten by
resigning, so at the winning moment every investigator is eliminated.
-}
module Arkham.Campaign.Campaigns.TheDrownedCity.Achievements (
  runDrownedCityAchievements,
) where

import Arkham.Achievement
import Arkham.Act.Cards qualified as Acts
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types qualified as Asset
import Arkham.Campaign.Types (campaignDifficulty)
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.TheDrownedCity.Helpers (rlyehArtifacts)
import Arkham.Campaigns.TheDrownedCity.Key
import Arkham.Card
import Arkham.Classes.Entity (toAttrs)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Difficulty
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyCard))
import Arkham.Game.Base
import Arkham.Game.Settings (activeUltimatumsAndBoons)
import Arkham.Helpers.Campaign (stored)
import Arkham.Helpers.Log (getHasRecord, getSomeRecordSet)
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid (GridLocation (..))
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Tracing
import Arkham.Trait (Trait (Cultist, Lift))
import Arkham.UltimatumsAndBoons.Types
import Data.Aeson.Key qualified as Key

runDrownedCityAchievements
  :: (HasGame m, HasQueue Message m, Tracing m) => Message -> m ()
runDrownedCityAchievements msg = whenEligibleCampaign $ case msg of
  -- Enemy defeats. The campaign sees Defeated before the enemy processes it, so
  -- the entity is still in play and queryable.
  Defeated (EnemyTarget eid) _ source _ -> do
    cardDef <- fieldMap EnemyCard toCardDef eid

    -- "This is a Coup": defeat both gang bosses in One Last Job using only the
    -- act 3a (Face the Music) parley ability, whose damage is dealt by the act's
    -- ability 1 — so the killing source unwraps to that act.
    when (cardDef `elem` gangBosses) $ whenScenarioIs oneLastJobId do
      when (isFaceTheMusicAbility source) do
        defeated <- storedList coupBossesKey
        let defeated' = nub (toCardCode cardDef : defeated)
        setStore coupBossesKey defeated'
        when (length defeated' >= length gangBosses) $ earn ThisIsACoup

    -- "Kill the Adds": defeat Mother without ever dealing her damage directly.
    -- Her Forced ability moves damage tokens off the Stowaway adds onto her,
    -- which arrives as PlaceTokens rather than Damaged, so a set damage flag
    -- means someone hit her the ordinary way.
    when (cardDef == Enemies.mother) $ whenScenarioIs theApiaryId do
      unlessM (storedFlag motherDamagedKey) $ earn KillTheAdds

    -- "Skip to the End": defeat Cthulhu in Sepulchre of the Sleeper. He is the
    -- back of Beneath the City, generated as an enemy when that agenda advances.
    when (cardDef == Enemies.cthulhuDeadAndDreaming) $ whenScenarioIs sepulchreId do
      earn SkipToTheEnd

  -- "Kill the Adds" bookkeeping: only genuine hits produce Damaged (Mother's
  -- own token transfer does not), so any damage here disqualifies the earn.
  Damaged (EnemyTarget eid) da | da.amount > 0 -> whenScenarioIs theApiaryId do
    cardDef <- fieldMap EnemyCard toCardDef eid
    when (cardDef == Enemies.mother) $ setStore motherDamagedKey True

  -- "WHY. WON'T. YOU. STAY. DEAD?!": The Inescapable carries Victory 0 and keeps
  -- coming back across the expedition, so every defeat adds it to the victory
  -- display again. Twenty of those in one campaign earns it.
  AddToVictory _ (EnemyTarget eid) -> do
    cardDef <- fieldMap EnemyCard toCardDef eid
    when (cardDef == Enemies.theInescapable) do
      n <- storedInt inescapableVictoriesKey
      setStore inescapableVictoriesKey (n + 1)
      when (n + 1 >= 20) $ earn WhyWontYouStayDead

  -- "Cliff Diver" bookkeeping: the Diving Suit only ever reaches play as an
  -- Expedition Item chosen during a scenario's setup, i.e. brought along.
  -- Controlled assets enter through CardEnteredPlay; PlaceAsset is matched too
  -- so no entry point is missed.
  CardEnteredPlay _ card
    | toCardCode card == toCardCode Assets.divingSuitTheDrownedCity ->
        setStore divingSuitKey True
  PlaceAsset aid _ -> do
    code <- fieldMap Asset.AssetCard toCardCode aid
    when (code == toCardCode Assets.divingSuitTheDrownedCity) $ setStore divingSuitKey True

  -- "No Acolyte Left Behind": rescuing a pilgrim is putting their Cultist card
  -- underneath the act (Apiary Entrance (Dangerous Exit) ability 1). There is no
  -- projection for an act's underneath pile, so the rescues are tallied here.
  PlaceUnderneath (ActTarget _) cards -> whenScenarioIs theApiaryId do
    let rescued = count (`cardMatch` CardWithTrait Cultist) cards
    when (rescued > 0) do
      n <- storedInt rescuedCultistsKey
      setStore rescuedCultistsKey (n + rescued)
      when (n + rescued >= 5) $ earn NoAcolyteLeftBehind

  -- "Sorry, Didn't See You There" bookkeeping: the Great Lift only ever changes
  -- level by being re-placed in the grid (both `slideGreatLift` and the
  -- antechambers' `slideGreatLiftDown` push PlaceGrid). Gated on the LATCHED
  -- spawn flag rather than the Tyrant still being in play, so killing it does
  -- not quietly reopen the window.
  PlaceGrid (GridLocation _ lid) -> whenScenarioIs courtOfTheAncientsId do
    whenM (storedFlag tyrantSpawnedKey) do
      isLift <- selectAny $ LocationWithId lid <> LocationWithTrait Lift
      when isLift $ setStore greatLiftMovedKey True

  {- Scenario-completion detections. These two want the scenario BEATEN, which
  cannot be inferred from surviving investigators: Escape the Tower's objective is
  literally "all undefeated investigators have resigned", so every investigator is
  eliminated at the winning moment. Key on the final act advancing instead — the
  campaign sees `AdvanceAct` before the act does, the board is fully intact, and
  the twin side-A/side-B dispatches only mean a harmless duplicate earn.
  -}
  AdvanceAct aid _ _
    | unActId aid == toCardCode Acts.stepsOfGiants ->
        -- Act 1 advancing is what fetches the Colossal Tyrant out of the set-aside pile.
        whenScenarioIs courtOfTheAncientsId $ setStore tyrantSpawnedKey True
  AdvanceAct aid _ _ | unActId aid `elem` map toCardCode escapeTheTowerActs ->
    whenScenarioIs courtOfTheAncientsId do
      unlessM (storedFlag greatLiftMovedKey) $ earn SorryDidntSeeYouThere
  AdvanceAct aid _ _ | unActId aid == toCardCode Acts.reactivateTheCore ->
    whenScenarioIs theDrownedQuarterId do
      -- "Tidal Flip Minigame": every location revealed and none flooded.
      unrevealed <- selectAny UnrevealedLocation
      flooded <- selectAny FloodedLocation
      unless (unrevealed || flooded) $ earn TidalFlipMinigame

  -- "Sky Rider": end five turns in open sky in a single game of Obsidian Canyons.
  EndTurn iid -> whenScenarioIs obsidianCanyonsId do
    whenM (selectAny $ locationIs Locations.openSky <> locationWithInvestigator iid) do
      n <- storedInt openSkyTurnEndsKey
      setStore openSkyTurnEndsKey (n + 1)
      when (n + 1 >= 5) $ earn SkyRider

  -- Per-game counters reset as their scenario is set up, so "during a single
  -- game" stays true even if a scenario is somehow revisited.
  Setup -> do
    whenScenarioIs theApiaryId $ setStore rescuedCultistsKey (0 :: Int)
    whenScenarioIs obsidianCanyonsId $ setStore openSkyTurnEndsKey (0 :: Int)
    whenScenarioIs courtOfTheAncientsId do
      setStore tyrantSpawnedKey False
      setStore greatLiftMovedKey False

  -- "Alien School Graduate": translate all 26 glyphs in one playthrough. The
  -- campaign log is updated by the same message we are dispatching on, so the
  -- new letters have to be folded into the set we read back.
  RecordSetInsert key entries | key == toCampaignLogKey DiscoveredGlyphs -> do
    existing <- getSomeRecordSet @Value DiscoveredGlyphs
    let glyphs = nub $ existing <> mapMaybe (unrecorded @Value) entries
    when (length glyphs >= 26) $ earn AlienSchoolGraduate

  -- Scenario-end detections, per scenario. See the module header for why these
  -- hang off EndOfGame rather than the resolution.
  EndOfGame _ ->
    selectOne TheScenario >>= traverse_ \sid ->
      if
        | sid == westernWallId ->
            -- "Thorough Search": end The Western Wall with every location revealed.
            unlessM (selectAny UnrevealedLocation) $ earn ThoroughSearch
        | sid == theGrandVaultId -> do
            -- "In The Deep End": escape holding the Tidal Tablet with the vault
            -- fully flooded. The Great Stair, the Moving Platform, the Core and the
            -- Chamber of the Tablet can never be flooded, so "every location fully
            -- flooded" is "nothing left that can take on more water". Artifacts pass
            -- to the nearest surviving investigator when their controller is
            -- defeated and are removed when nobody survives, so "still controlled"
            -- already excludes a total wipe.
            heldTablet <- selectAny $ assetIs Assets.tidalTablet <> AssetControlledBy Anyone
            anyDry <- selectAny $ Anywhere <> CanHaveFloodLevelIncreased
            when (heldTablet && not anyDry) $ earn InTheDeepEnd
        | otherwise -> pure ()
  -- "Empty Handed": return from R'lyeh (Interlude IV) with no Artifact earned.
  -- The Horror in Clay is only earned back in Arkham, so it is not one of these.
  CampaignStep (InterludeStep 4 _) -> do
    unlessM (anyM getHasRecord rlyehArtifacts) $ earn EmptyHanded

  -- End of campaign. Every surviving ending routes through the epilogue; the
  -- total loss (Cthulhu annihilates Arkham) calls gameOver instead and never
  -- gets here, so reaching this step is both "completed" and "won".
  CampaignStep EpilogueStep -> do
    earn OneFirstLastJob

    g <- getGame
    let difficulty = campaignDifficulty . toAttrs <$> currentCampaign (gameMode g)

    {- "Season Two" is deliberately NOT detected. It asks for four investigators
    carried into The Drowned City through Epic Campaign Mode, each having
    \*completed* a different previous campaign — the transfer keeps their trauma,
    story rewards and weaknesses, forfeits their experience, and records
    "<name>'s total experience earned: X" in the new campaign log. None of that
    exists in the engine yet (Arkham.Epic is Epic *Multiplayer*, a different
    feature), so there is nothing to key on: an investigator's provenance is
    simply not recorded anywhere. Deriving it from the investigators' card cycles
    was wrong and false-positived on any hard/expert four-player game whose picks
    happened to span four cycles. Left unearnable until Epic Campaign Mode lands.
    -}

    -- "Cliff Diver": never brought a Diving Suit along.
    unlessM (storedFlag divingSuitKey) $ earn CliffDiver

    -- "Alien School Dropout": finished without translating a single glyph.
    glyphs <- getSomeRecordSet @Value DiscoveredGlyphs
    when (null glyphs) $ earn AlienSchoolDropout

    -- "Line in the Sand": win with at least 3 Ultimatums active.
    let ultimatums = length [u | Ultimatum u <- toList $ activeUltimatumsAndBoons (gameSettings g)]
    when (ultimatums >= 3) $ earn DrownedCityLineInTheSand

    -- "R'lyeh Expertise": win on Expert.
    when (difficulty == Just Expert) $ earn RlyehExpertise

    -- The two checklist achievements. Both require finishing the campaign, so
    -- they are only reported here; the API layer accumulates the items across
    -- playthroughs and awards the earn once every box is checked.
    artifacts <- filterM (getHasRecord . fst) artifactItems
    achievementProgress (TheDrownedCityAchievement WithYourPowersCombined) (map snd artifacts)

    completed <- filterM (selectAny . IncludeEliminated . investigatorWithRecord . fst) taskItems
    achievementProgress (TheDrownedCityAchievement Obligations) (map snd completed)
  _ -> pure ()

earn :: (HasGame m, HasQueue Message m) => TheDrownedCityAchievement -> m ()
earn = earnAchievement . TheDrownedCityAchievement

{- | Gate the whole module (including store writes) to campaigns that can earn
these achievements. Derived from 'achievementCampaigns' so this cannot drift
from 'earnAchievement''s own campaign gate.
-}
whenEligibleCampaign :: HasGame m => m () -> m ()
whenEligibleCampaign body = do
  mCampaignId <- currentCampaignId
  let eligible = achievementCampaigns $ TheDrownedCityAchievement OneFirstLastJob
  when (maybe False (`elem` eligible) mCampaignId) body

whenScenarioIs :: (HasGame m, Tracing m) => ScenarioId -> m () -> m ()
whenScenarioIs sid body = do
  mSid <- selectOne TheScenario
  when (mSid == Just sid) body

oneLastJobId, westernWallId, theDrownedQuarterId, theApiaryId :: ScenarioId
oneLastJobId = "11501"
westernWallId = "11517"
theDrownedQuarterId = "11536"
theApiaryId = "11553"

theGrandVaultId, courtOfTheAncientsId, obsidianCanyonsId, sepulchreId :: ScenarioId
theGrandVaultId = "11587"
courtOfTheAncientsId = "11612"
obsidianCanyonsId = "11639"
sepulchreId = "11673"

-- | The two bosses "This is a Coup" wants defeated by the act 3a parley.
gangBosses :: [CardDef]
gangBosses = [Enemies.naomiOBannion, Enemies.sadieSheldon]

{- | Court of the Ancients' final act, whose advance IS beating the scenario (its
objective is "all undefeated investigators have resigned", so no investigator
liveness check can stand in for it). Which version is in play depends on the
direction the expedition took.
-}
escapeTheTowerActs :: [CardDef]
escapeTheTowerActs = [Acts.escapeTheTowerV1, Acts.escapeTheTowerV2]

{- | Whether a killing source unwraps to Face the Music's parley ability. An
'ActId' is the act's card code, so no projection is needed; there is no
@HasField "act"@ on 'Source', so the ability/payment wrappers are peeled here
the same way the built-in accessors peel them.
-}
isFaceTheMusicAbility :: Source -> Bool
isFaceTheMusicAbility source =
  maybe False ((== toCardCode Acts.faceTheMusic) . unActId) (sourceAct source)

sourceAct :: Source -> Maybe ActId
sourceAct = \case
  ActSource aid -> Just aid
  AbilitySource s _ -> sourceAct s
  UseAbilitySource _ s _ -> sourceAct s
  IndexedSource _ s -> sourceAct s
  ProxySource s _ -> sourceAct s
  PaymentSource s -> sourceAct s
  _ -> Nothing

-- Artifact record -> checklist item key, in 'achievementChecklist'
-- WithYourPowersCombined order.
artifactItems :: [(TheDrownedCityKey, Text)]
artifactItems =
  [ (BarrierNode, "BarrierNode")
  , (GrislyMask, "GrislyMask")
  , (ObsidianClaw, "ObsidianClaw")
  , (TidalTablet, "TidalTablet")
  , (ShardOfYchlecht, "ShardOfYchlecht")
  , (HorrorInClay, "HorrorInClay")
  ]

{- | Each Task's "completed" record (written per investigator by Interlude IV,
Return to Arkham) paired with its checklist item key. A single playthrough can
only complete as many Tasks as there are investigators, which is why this is a
cross-playthrough checklist.
-}
taskItems :: [(TheDrownedCityKey, Text)]
taskItems =
  [ (IsStrongInTheirFaith, "WalkInFaith")
  , (UnderstandsTheFuture, "DreamsOfDestruction")
  , (FoundNewWork, "ToeTheLine")
  , (SworeAnOathToProtectOthers, "DoNoHarm")
  , (MadeBank, "GoodMoney")
  , (FoundTheirTrueHome, "NoPlaceLikeHome")
  , (PulledTheirWeight, "ProveYourWorth")
  , (LearnedTheSecretTruth, "PlumbTheDepths")
  ]

-- Campaign store plumbing. Writes go through the queue ('SetGlobal' is handled
-- by the campaign runner); reads see all previously processed writes.

coupBossesKey
  , motherDamagedKey
  , rescuedCultistsKey
  , inescapableVictoriesKey
  , divingSuitKey
  , tyrantSpawnedKey
  , greatLiftMovedKey
  , openSkyTurnEndsKey
    :: Text
coupBossesKey = "tdcAchCoupBosses"
motherDamagedKey = "tdcAchMotherDamaged"
rescuedCultistsKey = "tdcAchRescuedCultists"
inescapableVictoriesKey = "tdcAchInescapableVictories"
divingSuitKey = "tdcAchDivingSuit"
tyrantSpawnedKey = "tdcAchTyrantSpawned"
greatLiftMovedKey = "tdcAchGreatLiftMoved"
openSkyTurnEndsKey = "tdcAchOpenSkyTurnEnds"

-- Priority so the write is applied before the rest of the triggering message's
-- cascade — some cascades (e.g. defeating a victory enemy) clearQueue, which
-- would otherwise drop a plainly-pushed store write.
setStore :: (HasQueue Message m, ToJSON a) => Text -> a -> m ()
setStore k v = push $ Priority $ SetGlobal CampaignTarget (Key.fromText k) (toJSON v)

storedInt :: (HasCallStack, HasGame m, Tracing m) => Text -> m Int
storedInt k = fromMaybe 0 <$> stored k

storedFlag :: (HasCallStack, HasGame m, Tracing m) => Text -> m Bool
storedFlag k = fromMaybe False <$> stored k

storedList :: (HasCallStack, HasGame m, Tracing m) => Text -> m [CardCode]
storedList k = fromMaybe [] <$> stored k

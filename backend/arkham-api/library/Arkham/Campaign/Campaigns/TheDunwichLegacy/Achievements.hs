{- | Return to the Dunwich Legacy achievement detection. Hooked from the
campaign's runMessage (campaign dispatch runs for every message, BEFORE the
scenario and other entities, so defeated enemies etc. are still queryable).
'earnAchievement' self-gates on the achievements setting and on the campaign
id ("51"), so earns stay unconditional here.

Campaign-persistent trackers live in the campaign store (via 'SetGlobal'
messages, read back with 'stored'); the whole module is additionally gated to
achievement-eligible campaigns so base Dunwich Legacy games don't accumulate
tracker keys in their store.
-}
module Arkham.Campaign.Campaigns.TheDunwichLegacy.Achievements (
  runDunwichAchievements,
) where

import Arkham.Achievement
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types qualified as Asset
import Arkham.Campaign.Types (campaignDifficulty)
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheDunwichLegacy.Key
import Arkham.Card (CardDef, toCardCode, toCardDef)
import Arkham.ChaosToken (ChaosTokenFace (..))
import Arkham.Classes.Entity (toAttrs)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Difficulty
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Game.Base
import Arkham.Game.Settings (activeUltimatumsAndBoons)
import Arkham.Helpers.Campaign (stored)
import Arkham.Helpers.ChaosBag (getAllChaosTokens)
import Arkham.Helpers.Log (getHasRecord, getRecordSet)
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Projection
import Arkham.Target
import Arkham.Tracing
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.UltimatumsAndBoons.Types
import Data.Aeson.Key qualified as Key

runDunwichAchievements
  :: (HasGame m, HasQueue Message m, Tracing m) => Message -> m ()
runDunwichAchievements msg = whenEligibleCampaign $ case msg of
  -- "First Rule of Arkham": the lead chose to burn the Necronomicon when act 4
  -- advanced (only The Miskatonic Museum's Resolution 1 writes this record).
  -- Either museum resolution (destroyed or took custody) completes the
  -- scenario, checking "No Void For You" against the defeat tracker below.
  Record key | key == toCampaignLogKey TheInvestigatorsDestroyedTheNecronomicon -> do
    earnAchievement $ TheDunwichLegacyAchievement FirstRuleOfArkham
    checkNoVoidForYou
  Record key | key == toCampaignLogKey TheInvestigatorsTookCustodyOfTheNecronomicon -> do
    checkNoVoidForYou

  -- "Eureka!": Strange Solution records this on the passed intellect test.
  Record YouHaveIdentifiedTheSolution -> do
    earnAchievement $ TheDunwichLegacyAchievement Eureka

  -- "No Brood Left Behind": Resolution 2 (all broods defeated) records the
  -- flag; Resolution 1 records a count, which also qualifies when it is zero.
  Record key | key == toCampaignLogKey NoBroodEscapedIntoTheWild -> do
    earnAchievement $ TheDunwichLegacyAchievement NoBroodLeftBehind
  RecordCount key 0 | key == toCampaignLogKey BroodEscapedIntoTheWild -> do
    earnAchievement $ TheDunwichLegacyAchievement NoBroodLeftBehind

  -- "The Gang's All Here": all five allies survive (recorded one by one in
  -- interlude 2, The Survivors). Earlier keys in the batch are already
  -- applied to the log when a later Record dispatches, so check the others.
  Record key | key `elem` map toCampaignLogKey survivedKeys -> do
    let others = filter ((/= key) . toCampaignLogKey) survivedKeys
    allSurvived <- and <$> traverse getHasRecord others
    when allSurvived do
      earnAchievement $ TheDunwichLegacyAchievement TheGangsAllHere

  -- Campaign win: Lost in Time and Space's Resolution 1 is the only
  -- resolution the investigators survive with a record.
  Record key | key == toCampaignLogKey TheInvestigatorsClosedTheTearInReality -> do
    -- "They Aren't Getting Away With This": win as "Ashcan" Pete with Duke
    -- sacrificed to Yog-Sothoth (Duke reaches the sacrifice set by being
    -- kidnapped and left under the agenda deck in Blood on the Altar).
    -- Everyone RESIGNS through the tear before this resolution, and plain
    -- investigator matchers exclude eliminated investigators — so include
    -- them, requiring only that Pete is still alive and sane.
    hasPete <- selectAny $ IncludeEliminated (InvestigatorIs "02005") <> AliveInvestigator
    dukeSacrificed <-
      elem "02014" . recordedCardCodes <$> getRecordSet SacrificedToYogSothoth
    when (hasPete && dukeSacrificed) do
      earnAchievement $ TheDunwichLegacyAchievement TheyArentGettingAwayWithThis

    -- "Tabula Rasa": no Tablet or Elder Thing tokens in the chaos bag
    -- (sealed tokens still count as the bag's contents).
    tokens <- getAllChaosTokens
    when (all ((`notElem` [Tablet, ElderThing]) . (.face)) tokens) do
      earnAchievement $ TheDunwichLegacyAchievement TabulaRasa

    g <- getGame
    -- "Dunwich Line in the Sand": win with at least 3 Ultimatums active.
    let ultimatums = length [u | Ultimatum u <- toList $ activeUltimatumsAndBoons (gameSettings g)]
    when (ultimatums >= 3) do
      earnAchievement $ TheDunwichLegacyAchievement DunwichLineInTheSand
    -- "Dunwich Expertise": win on Expert.
    let mDifficulty = campaignDifficulty . toAttrs <$> currentCampaign (gameMode g)
    when (mDifficulty == Just Expert) do
      earnAchievement $ TheDunwichLegacyAchievement DunwichExpertise

  -- Enemy defeats. The campaign sees Defeated before the enemy processes it,
  -- so the entity is still in play and queryable.
  Defeated (EnemyTarget eid) _ source _ -> do
    cardDef <- fieldMap EnemyCard toCardDef eid

    -- "What Is This Stuff, Anyway?": the Alchemical Concoction's fight
    -- ability dealt the killing blow to The Experiment (the Defeated source
    -- is the ability source on the asset). The concoction removes itself
    -- from the game on the passed test, which can process before the defeat
    -- lands — so resolve the card via projection (which still sees
    -- action-removed entities) rather than an in-play matcher.
    when (cardDef == Enemies.theExperiment) do
      for_ source.asset \aid -> do
        mCard <- fieldMay Asset.AssetCard aid
        let isConcoction = any ((== toCardCode Assets.alchemicalConcoction) . toCardCode) mCard
        when isConcoction do
          earnAchievement $ TheDunwichLegacyAchievement WhatIsThisStuffAnyway

    -- "No Void For You": any Hunting Horror defeat (even though it returns
    -- to the void rather than being discarded, the defeat still happens).
    when (cardDef == Enemies.huntingHorror) do
      setStore huntingHorrorDefeatedKey True

    -- "Remind Me Not To Piss Her Off": either Seth Bishop, while Naomi
    -- O'Bannion (only obtainable in Return to Where Doom Awaits) is under an
    -- investigator's control.
    when (cardDef `elem` [Enemies.sethBishop, Enemies.sethBishopThrallOfYogSothoth]) do
      whenM (selectAny $ assetIs Assets.naomiOBannionRuthlessTactician <> AssetControlledBy Anyone) do
        earnAchievement $ TheDunwichLegacyAchievement RemindMeNotToPissHerOff

    -- "Bird Hunting": 3 Whippoorwills defeated in a single turn (counter
    -- resets on the turn boundaries below).
    when (cardDef == Enemies.whippoorwill) do
      kills <- storedInt whippoorwillKillsKey
      setStore whippoorwillKillsKey (kills + 1)
      when (kills + 1 >= 3) do
        earnAchievement $ TheDunwichLegacyAchievement BirdHunting
  BeginTurn _ -> setStore whippoorwillKillsKey (0 :: Int)
  EndTurn _ -> setStore whippoorwillKillsKey (0 :: Int)
  -- "All Aboard": no Helpless Passenger may leave play in The Essex County
  -- Express. Every leave-play path (defeat, or its train car being removed)
  -- routes through a Discard of the asset; rescuing (taking control) does
  -- not. AssetDefeated is matched too in case a defeat is routed directly.
  Discard _ _ (AssetTarget aid) -> whenEssexCountyExpress $ flagPassengerLeft aid
  Msg.AssetDefeated _ aid -> whenEssexCountyExpress $ flagPassengerLeft aid
  -- Completion is detected on Essex's DoStep re-entry, NOT on
  -- ScenarioResolution itself: the Scenario wrapper clearQueues twice while
  -- processing ScenarioResolution (entering resolution and re-processing),
  -- which wipes anything the campaign pushes during that message.
  DoStep 1 (ScenarioResolution _) -> whenEssexCountyExpress do
    leftPlay <- fromMaybe False <$> stored @Bool passengerLeftPlayKey
    unless leftPlay do
      earnAchievement $ TheDunwichLegacyAchievement AllAboard

  -- "Here We Go Again": the three kidnappable professors in play at once.
  -- Controlled copies enter through CardEnteredPlay; uncontrolled story
  -- copies placed at locations only get PlaceAsset, so match both (the
  -- entering asset is already in the entity map when either dispatches).
  CardEnteredPlay _ card | toCardCode card `elem` map toCardCode professors -> do
    checkHereWeGoAgain
  PlaceAsset aid _ -> do
    code <- fieldMap Asset.AssetCard toCardCode aid
    when (code `elem` map toCardCode professors) checkHereWeGoAgain

  -- "Beyond What Veil?": Beyond the Veil's full 10 damage resolved against
  -- you (soaking onto your assets still counts as taking the hit; a canceled
  -- or reduced hit does not) and you survived. This is the terminal
  -- assignment message: each point was applied as it was assigned, so
  -- remaining health already reflects the hit and >0 means survival.
  InvestigatorDoAssignDamage iid source _ _ 0 0 damageTargets _ -> do
    for_ source.treachery \tid -> do
      isBeyondTheVeil <- selectAny $ TreacheryWithId tid <> treacheryIs Treacheries.beyondTheVeil
      when (isBeyondTheVeil && length damageTargets >= 10) do
        remaining <- field InvestigatorRemainingHealth iid
        when (remaining > 0) do
          earnAchievement $ TheDunwichLegacyAchievement BeyondWhatVeil
  _ -> pure ()

{- | Gate the whole module (including store writes) to campaigns that can earn
these achievements. Derived from 'achievementCampaigns' so this cannot
drift from 'earnAchievement''s own campaign gate.
-}
whenEligibleCampaign :: HasGame m => m () -> m ()
whenEligibleCampaign body = do
  mCampaignId <- currentCampaignId
  let eligible = achievementCampaigns $ TheDunwichLegacyAchievement AllAboard
  when (maybe False (`elem` eligible) mCampaignId) body

checkHereWeGoAgain :: (HasGame m, HasQueue Message m, Tracing m) => m ()
checkHereWeGoAgain = do
  allInPlay <- and <$> traverse (selectAny . assetIs) professors
  when allInPlay do
    earnAchievement $ TheDunwichLegacyAchievement HereWeGoAgain

checkNoVoidForYou :: (HasGame m, HasQueue Message m, Tracing m) => m ()
checkNoVoidForYou = do
  defeated <- fromMaybe False <$> stored @Bool huntingHorrorDefeatedKey
  unless defeated do
    earnAchievement $ TheDunwichLegacyAchievement NoVoidForYou

flagPassengerLeft :: (HasGame m, HasQueue Message m, Tracing m) => AssetId -> m ()
flagPassengerLeft aid =
  whenM (selectAny $ AssetWithId aid <> assetIs Assets.helplessPassenger) do
    setStore passengerLeftPlayKey True

whenEssexCountyExpress :: (HasGame m, Tracing m) => m () -> m ()
whenEssexCountyExpress body = do
  mSid <- selectOne TheScenario
  when (maybe False (`elem` theEssexCountyExpressIds) mSid) body

theEssexCountyExpressIds :: [ScenarioId]
theEssexCountyExpressIds = ["02159", "51025"]

survivedKeys :: [TheDunwichLegacyKey]
survivedKeys =
  [ DrHenryArmitageSurvivedTheDunwichLegacy
  , ProfessorWarrenRiceSurvivedTheDunwichLegacy
  , DrFrancisMorganSurvivedTheDunwichLegacy
  , ZebulonWhateleySurvivedTheDunwichLegacy
  , EarlSawyerSurvivedTheDunwichLegacy
  ]

professors :: [CardDef]
professors = [Assets.drHenryArmitage, Assets.professorWarrenRice, Assets.drFrancisMorgan]

-- Campaign store plumbing. Writes go through the queue ('SetGlobal' is
-- handled by the campaign runner); reads see all previously processed writes.

huntingHorrorDefeatedKey, passengerLeftPlayKey, whippoorwillKillsKey :: Text
huntingHorrorDefeatedKey = "dunwichAchHuntingHorrorDefeated"
passengerLeftPlayKey = "dunwichAchPassengerLeftPlay"
whippoorwillKillsKey = "dunwichAchWhippoorwillKills"

setStore :: (HasQueue Message m, ToJSON a) => Text -> a -> m ()
setStore k v = push $ SetGlobal CampaignTarget (Key.fromText k) (toJSON v)

storedInt :: (HasCallStack, HasGame m, Tracing m) => Text -> m Int
storedInt k = fromMaybe 0 <$> stored k

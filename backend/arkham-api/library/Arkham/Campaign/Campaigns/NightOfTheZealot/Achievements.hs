{- | Return to the Night of the Zealot achievement detection. Hooked from the
campaign's runMessage (campaign dispatch runs for every message, BEFORE the
scenario and other entities, so defeated enemies etc. are still queryable).
'earnAchievement' self-gates on the achievements setting and on the campaign
id ("50"), so earns stay unconditional here.

Campaign-persistent trackers live in the campaign store (via 'SetGlobal'
messages, read back with 'stored'); the whole module is additionally gated to
achievement-eligible campaigns so base Night of the Zealot games don't
accumulate tracker keys in their store.
-}
module Arkham.Campaign.Campaigns.NightOfTheZealot.Achievements (
  runNotzAchievements,
) where

import Arkham.Achievement
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Types (campaignDifficulty)
import Arkham.CampaignLogKey
import Arkham.Campaigns.NightOfTheZealot.Key
import Arkham.Card (CardCode, cdUnique, toCardCode, toCardDef)
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
import Arkham.Helpers.Modifiers (getFullModifiers)
import Arkham.Helpers.SkillTest (getSkillTestInvestigator)
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Movement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Target
import Arkham.Tracing
import Arkham.Trait (Trait (Cultist, Ghoul))
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.UltimatumsAndBoons.Types
import Data.Aeson.Key qualified as Key

runNotzAchievements
  :: (HasGame m, HasQueue Message m, Tracing m) => Message -> m ()
runNotzAchievements msg = whenEligibleCampaign $ case msg of
  -- "Insurance Doesn't Cover Ghouls": burn your house down in The Gathering.
  -- The Gathering's Resolution 1 is the only writer of this record.
  Record key | key == toCampaignLogKey YourHouseHasBurnedToTheGround -> do
    earnAchievement $ NightOfTheZealotAchievement InsuranceDoesntCoverGhouls

  -- Campaign win: The Devourer Below ends via Resolution 1/2/3, each of which
  -- records a distinct key (NoResolution records Arkham's doom instead). We
  -- key off the records rather than ScenarioResolution so no scenario-id
  -- bookkeeping is needed.
  Record key | key `elem` devourerBelowWinRecords -> do
    -- "Break the Circle": complete the scenario without Umordhoth spawning.
    -- Resolutions 2/3 require him in play, and once spawned he cannot leave
    -- play before the scenario ends, so "never spawned" is exactly "not in
    -- play at the ritual-broken resolution" (set-aside enemies don't match).
    when (key == toCampaignLogKey TheRitualToSummonUmordhothWasBroken) do
      whenM (selectNone $ enemyIs Enemies.umordhoth) do
        earnAchievement $ NightOfTheZealotAchievement BreakTheCircle

    -- "They're Just Misunderstood" / "Umordhoth's Favor": campaign-long
    -- defeat counters maintained in the Defeated hook below.
    whenM ((== 0) <$> storedInt uniqueCultistsDefeatedKey) do
      earnAchievement $ NightOfTheZealotAchievement TheyreJustMisunderstood
    whenM ((== 0) <$> storedInt ghoulsDefeatedKey) do
      earnAchievement $ NightOfTheZealotAchievement UmordhothsFavor

    g <- getGame
    -- "Zealot Line in the Sand": win with at least 3 Ultimatums active.
    let ultimatums = length [u | Ultimatum u <- toList $ activeUltimatumsAndBoons (gameSettings g)]
    when (ultimatums >= 3) do
      earnAchievement $ NightOfTheZealotAchievement ZealotLineInTheSand
    -- "Arkham Expertise": win on Expert.
    let mDifficulty = campaignDifficulty . toAttrs <$> currentCampaign (gameMode g)
    when (mDifficulty == Just Expert) do
      earnAchievement $ NightOfTheZealotAchievement ArkhamExpertise

  -- "Conspiracy of Silence": interrogate all six unique cultists in The
  -- Midnight Masks. Per the resolutions, "interrogated" means "unique Cultist
  -- in the victory display", and exactly six unique Cultists exist in a given
  -- game (five in the cultist deck plus The Masked Hunter), so a six-name
  -- insert is a full sweep. Only this scenario writes the key.
  RecordSetInsert key entries | key == toCampaignLogKey CultistsWeInterrogated -> do
    when (length (nub $ recordedCardCodes entries) >= 6) do
      earnAchievement $ NightOfTheZealotAchievement ConspiracyOfSilence

  -- "I Don't Trust Her" is earned at the offer itself: The Gathering's
  -- resolution attaches the earn to the decline branch of the add-Lita choice
  -- (addCampaignCardToDeckChoiceWhenDeclined in TheGathering.hs).

  -- Enemy defeats: campaign-long counters plus the defeat-shaped achievements.
  -- The campaign sees Defeated before the enemy processes it, so the entity is
  -- still in play and queryable.
  Defeated (EnemyTarget eid) _ source traits -> do
    cardDef <- fieldMap EnemyCard toCardDef eid

    when (Ghoul `elem` traits) do
      bumpCounter ghoulsDefeatedKey
      -- "Pinch Hitter": 3 Ghouls with one Baseball Bat, without it breaking.
      -- Kills are counted per asset id; a broken (discarded) bat's id simply
      -- never reaches 3, and a re-played copy enters with a fresh id.
      for_ source.asset \aid -> do
        isBat <- selectAny $ AssetWithId aid <> mapOneOf assetIs [Assets.baseballBat, Assets.baseballBat2]
        when isBat do
          kills <- fromMaybe mempty <$> stored @(Map Text Int) baseballBatKillsKey
          let kills' = insertWith (+) (tshow aid) 1 kills
          setStore baseballBatKillsKey kills'
          when (findWithDefault 0 (tshow aid) kills' >= 3) do
            earnAchievement $ NightOfTheZealotAchievement PinchHitter

    when (Cultist `elem` traits && cdUnique cardDef) do
      bumpCounter uniqueCultistsDefeatedKey

    when (cardDef == Enemies.ghoulPriest) do
      -- "The Zealot's Revenge": Lita Chantler's reaction (+1 damage while the
      -- attack's skill test resolves) deals the killing blow, i.e. without her
      -- bonus the total damage would not have reached his (modified) health.
      getSkillTestInvestigator >>= traverse_ \iid -> do
        litas <- select $ assetIs Assets.litaChantler
        mods <- getFullModifiers iid
        let litaBonus = sum [n | Modifier src (DamageDealt n) _ _ <- mods, maybe False (`elem` litas) src.asset]
        when (litaBonus > 0) do
          dmg <- field EnemyDamage eid
          mHealth <- field EnemyHealth eid
          when (maybe False (\h -> dmg - litaBonus < h) mHealth) do
            earnAchievement $ NightOfTheZealotAchievement TheZealotsRevenge
      -- "Do You Get It Now?": Billy Cooper's Forced ability triggers off any
      -- Monster defeated at his location; the achievement wants specifically
      -- the Ghoul Priest, so check Billy is at the priest's location.
      field EnemyLocation eid >>= traverse_ \lid -> do
        whenM (selectAny $ enemyIs Enemies.billyCooper <> EnemyAt (LocationWithId lid)) do
          earnAchievement $ NightOfTheZealotAchievement DoYouGetItNow

    -- "Even Death May Die": defeat Umordhoth with Vault of Earthly Demise
    -- attached (in Return to The Devourer Below it always is).
    when (cardDef == Enemies.umordhoth) do
      whenM
        (selectAny $ treacheryIs Treacheries.vaultOfEarthlyDemise <> TreacheryIsAttachedTo (EnemyTarget eid))
        do
          earnAchievement $ NightOfTheZealotAchievement EvenDeathMayDie

  -- "Tour of Arkham": trigger each once-per-game location ability on all
  -- locations in play in The Midnight Masks. All such abilities are printed
  -- ability 1 on their location.
  UseCardAbility _ source 1 _ _ -> whenMidnightMasks $ for_ source.location \lid -> do
    code <- field LocationCardCode lid
    when (code `elem` oncePerGameLocationCodes) do
      used <- fromMaybe [] <$> stored @[CardCode] tourOfArkhamKey
      let used' = nub (code : used)
      setStore tourOfArkhamKey used'
      inPlay <- selectField LocationCardCode Anywhere
      let required = filter (`elem` oncePerGameLocationCodes) (nub inPlay)
      when (notNull required && all (`elem` used') required) do
        earnAchievement $ NightOfTheZealotAchievement TourOfArkham

  -- "But Do I Have To?": leaving Your House mid-round during the first three
  -- rounds also breaks the achievement, not just being elsewhere at round end.
  -- Movement enters as Move and resolves through MoveTo; match both so no
  -- entry point is missed. ponytail: exotic relocations (direct placement)
  -- are only caught by the end-of-round check.
  Move movement -> checkLeftYourHouse movement
  MoveTo movement -> checkLeftYourHouse movement
  -- "But Do I Have To?": every investigator stays in Your House for the first
  -- three rounds of The Midnight Masks (so the house must be in play, i.e. it
  -- didn't burn down in The Gathering).
  EndRound -> whenMidnightMasks do
    rounds <- storedInt stayedHomeRoundsKey
    when (rounds < 3) do
      houseInPlay <- selectAny $ locationIs Locations.yourHouse
      violation <-
        selectAny $ UneliminatedInvestigator <> not_ (InvestigatorAt (locationIs Locations.yourHouse))
      leftHome <- fromMaybe False <$> stored @Bool leftHomeKey
      if houseInPlay && not violation && not leftHome
        then do
          setStore stayedHomeRoundsKey (rounds + 1)
          when (rounds + 1 == 3) do
            earnAchievement $ NightOfTheZealotAchievement ButDoIHaveTo
        else setStore leftHomeKey True
  _ -> pure ()

{- | Gate the whole module (including store writes) to campaigns that can earn
these achievements. Derived from 'achievementCampaigns' so this cannot
drift from 'earnAchievement''s own campaign gate.
-}
whenEligibleCampaign :: HasGame m => m () -> m ()
whenEligibleCampaign body = do
  mCampaignId <- currentCampaignId
  let eligible = achievementCampaigns $ NightOfTheZealotAchievement TheZealotsRevenge
  when (maybe False (`elem` eligible) mCampaignId) body

checkLeftYourHouse
  :: (HasGame m, HasQueue Message m, Tracing m) => Movement -> m ()
checkLeftYourHouse movement = whenMidnightMasks $ case (movement.target, movement.destination) of
  (InvestigatorTarget _, ToLocation lid) -> do
    rounds <- storedInt stayedHomeRoundsKey
    when (rounds < 3) do
      isHouse <- selectAny $ LocationWithId lid <> locationIs Locations.yourHouse
      unless isHouse $ setStore leftHomeKey True
  _ -> pure ()

whenMidnightMasks :: (HasGame m, Tracing m) => m () -> m ()
whenMidnightMasks body = do
  mSid <- selectOne TheScenario
  when (maybe False (`elem` theMidnightMasksIds) mSid) body

theMidnightMasksIds :: [ScenarioId]
theMidnightMasksIds = ["01120", "50025"]

devourerBelowWinRecords :: [CampaignLogKey]
devourerBelowWinRecords =
  map
    toCampaignLogKey
    [ TheRitualToSummonUmordhothWasBroken
    , TheInvestigatorsRepelledUmordoth
    , TheInvestigatorsSacrificedLitaChantlerToUmordhoth
    ]

{- | Every (Return to) The Midnight Masks location with a printed
once-per-game ability. Locations without one (Rivertown, Graveyard, base
Miskatonic University, base Easttown, Your House) impose no requirement.
-}
oncePerGameLocationCodes :: [CardCode]
oncePerGameLocationCodes =
  map
    toCardCode
    [ Locations.northside
    , Locations.northsideTrainStation
    , Locations.downtownFirstBankOfArkham
    , Locations.downtownArkhamAsylum
    , Locations.easttownArkhamPoliceStation
    , Locations.miskatonicUniversityMiskatonicMuseum
    , Locations.rivertownAbandonedWarehouse
    , Locations.southsideHistoricalSociety
    , Locations.southsideMasBoardingHouse
    , Locations.stMarysHospital
    ]

-- Campaign store plumbing. Writes go through the queue ('SetGlobal' is
-- handled by the campaign runner); reads see all previously processed writes.

ghoulsDefeatedKey
  , uniqueCultistsDefeatedKey
  , stayedHomeRoundsKey
  , leftHomeKey
  , tourOfArkhamKey
  , baseballBatKillsKey
    :: Text
ghoulsDefeatedKey = "notzAchGhoulsDefeated"
uniqueCultistsDefeatedKey = "notzAchUniqueCultistsDefeated"
stayedHomeRoundsKey = "notzAchStayedHomeRounds"
leftHomeKey = "notzAchLeftHome"
tourOfArkhamKey = "notzAchTourOfArkham"
baseballBatKillsKey = "notzAchBaseballBatKills"

setStore :: (HasQueue Message m, ToJSON a) => Text -> a -> m ()
setStore k v = push $ SetGlobal CampaignTarget (Key.fromText k) (toJSON v)

storedInt :: (HasCallStack, HasGame m, Tracing m) => Text -> m Int
storedInt k = fromMaybe 0 <$> stored k

bumpCounter :: (HasCallStack, HasGame m, HasQueue Message m, Tracing m) => Text -> m ()
bumpCounter k = do
  n <- storedInt k
  setStore k (n + 1)

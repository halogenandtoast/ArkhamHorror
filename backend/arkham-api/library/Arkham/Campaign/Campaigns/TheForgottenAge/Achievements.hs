{- | Return to The Forgotten Age achievement detection. Hooked from the
campaign's runMessage (campaign dispatch runs for every message, BEFORE the
scenario and other entities, so defeated enemies etc. are still queryable).
'earnAchievement' self-gates on the achievements setting and on the campaign
id ("53"), so earns stay unconditional here.

Campaign-persistent trackers live in the campaign store (via 'SetGlobal'
messages, read back with 'stored'); the whole module is additionally gated to
achievement-eligible campaigns so base Forgotten Age games don't accumulate
tracker keys in their store.

The Return-to campaign inherits this hook by delegating every message except
'NextCampaignStep' to the base campaign runner; none of the detections below
key on 'NextCampaignStep', so all of them reach this module.
-}
module Arkham.Campaign.Campaigns.TheForgottenAge.Achievements (
  runForgottenAgeAchievements,
) where

import Arkham.Achievement
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Types (campaignDifficulty)
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.Key
import Arkham.Card (CardDef, toCardCode, toCardDef)
import Arkham.Classes.Entity (toAttrs)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Difficulty
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyCard))
import Arkham.Game.Base
import Arkham.Helpers.Campaign (stored)
import Arkham.Helpers.Log (getHasCrossedOutRecord, getHasRecord, getRecordCount, remembered)
import Arkham.Helpers.Scenario (resignedWith)
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Target
import Arkham.Tracing
import Arkham.Trait (Trait (Serpent))
import Arkham.Treachery.Cards qualified as Treacheries
import Data.Aeson.Key qualified as Key

runForgottenAgeAchievements
  :: (HasGame m, HasQueue Message m, Tracing m) => Message -> m ()
runForgottenAgeAchievements msg = whenEligibleCampaign $ case msg of
  -- Enemy defeats. The campaign sees Defeated before the enemy processes it,
  -- so the entity is still in play and queryable.
  Defeated (EnemyTarget eid) _ _ traits -> do
    -- "Why Did It Have to Be Snakes?": defeat twenty Serpent enemies across
    -- the campaign (Yig himself carries the Serpent trait, so he counts too).
    when (Serpent `elem` traits) do
      n <- storedInt serpentsDefeatedKey
      setStore serpentsDefeatedKey (n + 1)
      when (n + 1 >= 20) $ earn WhyDidItHaveToBeSnakes

    -- "Patricide": defeat Yig in The Depths of Yoth.
    whenDepthsOfYoth do
      cardDef <- fieldMap EnemyCard toCardDef eid
      when (cardDef == Enemies.yig) $ earn Patricide

  -- "We Have an Understanding" bookkeeping: any real damage dealt to the
  -- Harbinger of Valusia disqualifies the win. The Harbinger flees between
  -- scenarios carrying its damage, but that damage is restored via
  -- 'placeTokens', not a 'Damaged' message, so this only fires on genuine hits.
  Damaged (EnemyTarget eid) da | da.amount > 0 -> do
    cardDef <- fieldMap EnemyCard toCardDef eid
    when (cardDef `elem` harbingerForms) $ setStore dealtHarbingerDamageKey True

  -- "I've Built Up An Immunity" bookkeeping: becoming poisoned always routes
  -- through creating the Poisoned weakness in a threat area.
  CreateWeaknessInThreatArea card _ | toCardCode card == toCardCode Treacheries.poisoned -> do
    setStore becamePoisonedKey True

  -- "Who Needs Any of This Junk?" bookkeeping: purchasing a supply (at the
  -- prologue or a Resupply Point) pushes PickSupply.
  PickSupply _ _ -> setStore boughtSupplyKey True

  -- "Scenario 5-What?": skip Heart of the Elders, Part 1. There is no record
  -- for the skip; Part 1's setup short-circuits (pushing R1 with no play) when
  -- 6 paths are already known. Detect that condition at Part 1 setup. Only the
  -- Return-to Part 1 scenario has a distinct id (the base Part 1/Part 2 share
  -- "04205a"), and this list is Return-to only anyway.
  Setup -> whenReturnHeartOfTheEldersPart1 do
    paths <- getRecordCount PathsAreKnownToYou
    when (paths == 6) $ earn Scenario5What

  -- "Watch Them Unravel": complete all four Threads of Fate act decks. The
  -- three "always recorded" outcomes and the conditional fourth-expedition
  -- record are written in one resolution batch; the fourth is last and only
  -- present when its deck completed, so keying on it and checking the other
  -- three (already applied) means all four completed.
  Record key | key == toCampaignLogKey TheInvestigatorsRecruitedTheHelpOfAnotherExpedition -> do
    others <-
      traverse
        getHasRecord
        [ TheInvestigatorsFoundTheMissingRelic
        , TheInvestigatorsRescuedAlejandro
        , TheInvestigatorsForgedABondWithIchtaca
        ]
    when (and others) $ earn WatchThemUnravel

  -- "Hope for Humanity": Ichtaca's faith restored (Interlude 3 records it).
  Record key | key == toCampaignLogKey IchtacasFaithIsRestored -> do
    earn HopeForHumanity

  -- "I Remember Everything!": Alejandro's memories restored (Interlude 4).
  Record key | key == toCampaignLogKey AlejandroRemembersEverything -> do
    earn IRememberEverything

  -- "Beyond Perfection": complete all eight tasks on act 2 of The City of
  -- Archives. TheProcessWasPerfected is recorded for 6+ tasks, so re-derive
  -- the exact total (seven Remember tasks + the Custodian) and require eight.
  Record key | key == toCampaignLogKey TheProcessWasPerfected -> do
    tasks <- countM remembered cityOfArchivesTasks
    custodian <-
      orM
        [ resignedWith Assets.theCustodian
        , selectAny (AssetControlledBy Anyone <> assetIs Assets.theCustodian)
        ]
    when (tasks + (if custodian then 1 else 0) == 8) $ earn BeyondPerfection

  -- Campaign wins. Shattered Aeons Resolutions 1/2/3 and Turn Back Time
  -- Resolution 1 are the surviving wins; each writes exactly one of these
  -- records. Siding with Alejandro (Yithians) / Ichtaca (Serpents) are wins
  -- too, so their side-specific achievements fold in here.
  Record key | key `elem` map toCampaignLogKey winRecords -> do
    -- "He's Got a Point" / "Valusia Sounds Great": which side you took.
    when (key == toCampaignLogKey TheInvestigatorsSavedTheCivilizationOfTheYithians) do
      earn HesGotAPoint
    when (key == toCampaignLogKey TheInvestigatorsSavedTheCivilizationOfTheSerpents) do
      earn ValusiaSoundsGreat
    -- "If I Could Turn Back Time": win via Turn Back Time (only reachable by
    -- forging your own path and unlocking Scenario IX). Turn Back Time's setup
    -- crosses out every ordered key except the first three, so by the time the
    -- win record lands YouAreForgingYourOwnWay is crossed out rather than an
    -- active record — accept either.
    when (key == toCampaignLogKey TheInvestigatorsSealedTheRelicOfAgesForever) do
      forged <-
        orM
          [ getHasRecord YouAreForgingYourOwnWay
          , getHasCrossedOutRecord YouAreForgingYourOwnWay
          ]
      when forged $ earn IfICouldTurnBackTime

    -- "I've Built Up An Immunity": never became poisoned.
    unlessM (storedFlag becamePoisonedKey) $ earn IveBuiltUpAnImmunity
    -- "We Have an Understanding": never damaged the Harbinger.
    unlessM (storedFlag dealtHarbingerDamageKey) $ earn WeHaveAnUnderstanding
    -- "Who Needs Any of This Junk?": never purchased a supply.
    unlessM (storedFlag boughtSupplyKey) $ earn WhoNeedsAnyOfThisJunk

    yigsFury <- getRecordCount YigsFury
    -- "Don't Tread on Me": no Yig's Fury.
    when (yigsFury == 0) $ earn DontTreadOnMe
    -- "Bane of Yig": 25+ Yig's Fury.
    when (yigsFury >= 25) $ earn BaneOfYig

    g <- getGame
    -- "Yoth Expertise": win on Expert.
    let mDifficulty = campaignDifficulty . toAttrs <$> currentCampaign (gameMode g)
    when (mDifficulty == Just Expert) $ earn YothExpertise
  _ -> pure ()

earn :: (HasGame m, HasQueue Message m) => TheForgottenAgeAchievement -> m ()
earn = earnAchievement . TheForgottenAgeAchievement

{- | Gate the whole module (including store writes) to campaigns that can earn
these achievements. Derived from 'achievementCampaigns' so this cannot drift
from 'earnAchievement''s own campaign gate.
-}
whenEligibleCampaign :: HasGame m => m () -> m ()
whenEligibleCampaign body = do
  mCampaignId <- currentCampaignId
  let eligible = achievementCampaigns $ TheForgottenAgeAchievement WhyDidItHaveToBeSnakes
  when (maybe False (`elem` eligible) mCampaignId) body

whenScenarioIn :: (HasGame m, Tracing m) => [ScenarioId] -> m () -> m ()
whenScenarioIn sids body = do
  mSid <- selectOne TheScenario
  when (maybe False (`elem` sids) mSid) body

whenDepthsOfYoth :: (HasGame m, Tracing m) => m () -> m ()
whenDepthsOfYoth = whenScenarioIn ["04277", "53059"]

whenReturnHeartOfTheEldersPart1 :: (HasGame m, Tracing m) => m () -> m ()
whenReturnHeartOfTheEldersPart1 = whenScenarioIn ["53045"]

-- Winning campaign-log records (the surviving Shattered Aeons resolutions plus
-- the Turn Back Time win).
winRecords :: [TheForgottenAgeKey]
winRecords =
  [ TheInvestigatorsMendedTheTearInTheFabricOfTime
  , TheInvestigatorsSavedTheCivilizationOfTheSerpents
  , TheInvestigatorsSavedTheCivilizationOfTheYithians
  , TheInvestigatorsSealedTheRelicOfAgesForever
  ]

harbingerForms :: [CardDef]
harbingerForms = [Enemies.harbingerOfValusia, Enemies.harbingerOfValusiaTheSleeperReturns]

-- The seven "Remember" tasks on act 2 of The City of Archives (the eighth is
-- the Custodian, tracked separately).
cityOfArchivesTasks :: [ScenarioLogKey]
cityOfArchivesTasks =
  [ FoundTheProcess
  , DissectedAnOrgan
  , InterviewedASubject
  , RealizedWhatYearItIs
  , ActivatedTheDevice
  , ReadAboutEarth
  , SawAFamiliarSpecimen
  ]

-- Campaign store plumbing. Writes go through the queue ('SetGlobal' is handled
-- by the campaign runner); reads see all previously processed writes.

serpentsDefeatedKey, becamePoisonedKey, dealtHarbingerDamageKey, boughtSupplyKey :: Text
serpentsDefeatedKey = "tfaAchSerpentsDefeated"
becamePoisonedKey = "tfaAchBecamePoisoned"
dealtHarbingerDamageKey = "tfaAchDealtHarbingerDamage"
boughtSupplyKey = "tfaAchBoughtSupply"

-- Priority so the write is applied before the rest of the triggering message's
-- cascade — some cascades (e.g. defeating a victory enemy) clearQueue, which
-- would otherwise drop a plainly-pushed store write.
setStore :: (HasQueue Message m, ToJSON a) => Text -> a -> m ()
setStore k v = push $ Priority $ SetGlobal CampaignTarget (Key.fromText k) (toJSON v)

storedInt :: (HasCallStack, HasGame m, Tracing m) => Text -> m Int
storedInt k = fromMaybe 0 <$> stored k

storedFlag :: (HasCallStack, HasGame m, Tracing m) => Text -> m Bool
storedFlag k = fromMaybe False <$> stored k

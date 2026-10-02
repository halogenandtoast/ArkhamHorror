{- | Dark Matter achievement detection (campaign guide achievement list).

Hooked from the campaign's runMessage, so it sees every message before the
scenario and the entities. 'earnAchievement' self-gates on the achievements
setting and on the campaign id (":dark-matter"), so earns here stay
unconditional; the module as a whole is gated by 'whenEligibleCampaign'.

Two timing rules shape everything below. Resolutions are never keyed on
directly ('Arkham.Scenario' clearQueues twice while processing one); and
anything that reads the board -- the scanning deck, a story asset's damage --
has to run at 'EndOfGame', because the scenario is dropped from the game mode
at the 'EndOfScenario' that follows it, long before the epilogue.
-}
module Arkham.Homebrew.DarkMatter.Achievements (
  runDarkMatterAchievements,
) where

import Arkham.Achievement
import Arkham.Campaign.Types (Field (CampaignChaosBag), campaignDifficulty)
import Arkham.CampaignStep
import Arkham.Card
import Arkham.ChaosToken.Types
import Arkham.Classes.Entity (toAttrs)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Difficulty
import Arkham.Enemy.Types (Field (EnemyCard))
import Arkham.Game.Base
import Arkham.Game.Settings (activeUltimatumsAndBoons)
import Arkham.Helpers.Campaign (stored)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Log (getHasRecord, getRecordCount)
import Arkham.Homebrew.DarkMatter.AchievementDefs
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Helpers (getScanningDeck)
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Homebrew.DarkMatter.Traits (pattern Brain)
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Trait (Trait (Crew))
import Arkham.UltimatumsAndBoons.Types
import Data.Aeson.Key qualified as Key

runDarkMatterAchievements :: (HasGame m, HasQueue Message m) => Message -> m ()
runDarkMatterAchievements msg = whenEligibleCampaign $ case msg of
  Defeated (EnemyTarget eid) cid source _ -> do
    cardDef <- fieldMap EnemyCard toCardDef eid

    {- "Airlock Sequence": Escape Pod Bay's only ability defeats the enemy
    outright, so the killing source is an ability on the location itself. It is
    group-limited to three uses per game, which is exactly the ask. -}
    for_ (abilityLocation source) \lid ->
      whenM (lid <=~> locationIs Locations.escapePodBay) $ bumpCounter airlockDefeatsKey 1

    whenScenarioIs electricNightmareId do
      -- "Mental Fortitude": an ordinary damage kill; still in play here.
      when (isPrintingOf (toCardCode Enemies.shadowOfThoughts) cardDef) $ earn MentalFortitude

      {- "Brain Burn": THE BOOGEYMAN cannot be attacked or damaged -- the fourth
      Reintegrated story defeats it -- so this fires once, at the end. -}
      when (isPrintingOf (toCardCode Enemies.theBOOGEYMAN) cardDef) do
        spent <- storedInt cluesSpentKey
        threshold <- perPlayer 6
        when (spent < threshold) $ earn BrainBurn

    {- "Out of Time": the three copies share a card code and are told apart only
    by their card ids, so the defeats are collected as a set. A copy can sit
    facedown in a threat area and never be drawn, so this is a real "all three". -}
    when (isPrintingOf (toCardCode Enemies.houndOfTindalos) cardDef) do
      whenScenarioIs lostQuantumId $ insertGlobal houndsDefeatedKey cid

  {- "Untattered": Reconnected is the only act that pushes The Tatterdemalion's
  one winning resolution. -}
  AdvanceAct aid _ _ | unActId aid == toCardCode Acts.reconnected ->
    whenScenarioIs theTatterdemalionId do
      whenM (null <$> getScanningDeck) $ earn Untattered

  {- "Savior of Nostalgia": The Shadow of Earth advances only through its
  Entity-defeated objective (its other ability resolves the scenario without
  advancing), so this message alone proves the Entity went down. The crew is
  counted here rather than at EndOfGame because the act's own body then sweeps
  the crew still sitting in the scanning deck into the victory display. -}
  AdvanceAct aid _ _ | unActId aid == toCardCode Acts.theShadowOfEarth -> do
    rescued <- selectCount $ VictoryDisplayCardMatch $ basic $ CardWithTrait Crew
    aboard <- selectCount $ AssetWithTrait Crew <> AssetControlledBy Anyone
    when (rescued + aboard >= 5) $ earn SaviorOfNostalgia

  {- "Neuroscientist": Secrets of the Mind is the only act that ends Strange
  Moons. The brains are remembered as they are hurt rather than inspected at the
  end: a defeated Brain is removed from the game, taking its damage with it. -}
  AdvanceAct aid _ _ | unActId aid == toCardCode Acts.secretsOfTheMind ->
    whenScenarioIs strangeMoonsId do
      unlessM (storedFlag brainDamagedKey) do
        whenM (null <$> getScanningDeck) $ earn Neuroscientist
  DealAssetDamageWithCheck aid _ damage _ _
    | damage > 0 ->
        whenM (aid <=~> AssetWithTrait Brain) $ setStore brainDamagedKey True
  {- "Self Help": Unmasked's objective needs every copy of Your Other Self gone,
  and the only way one goes is defeat, so its advance proves that half. The
  window to watch for clue spending opens when the previous act advances. -}
  AdvanceAct aid _ _
    | unActId aid == toCardCode Acts.theManInThePallidMask ->
        whenScenarioIs theMachineInYellowId $ setStore selfHelpCluesKey False
  AdvanceAct aid _ _ | unActId aid == toCardCode Acts.unmasked ->
    whenScenarioIs theMachineInYellowId do
      unlessM (storedFlag selfHelpCluesKey) $ earn SelfHelp
  InvestigatorSpendClues _ n | n > 0 -> do
    whenScenarioIs theMachineInYellowId do
      whenM (selectAny $ ActWithStep 3) $ setStore selfHelpCluesKey True
    -- "Brain Burn" counts every clue spent over the scenario.
    whenScenarioIs electricNightmareId $ bumpCounter cluesSpentKey n

  -- Per-game counters reset as their scenario is set up.
  Setup -> do
    whenScenarioIs theTatterdemalionId $ setStore airlockDefeatsKey (0 :: Int)
    whenScenarioIs electricNightmareId $ setStore cluesSpentKey (0 :: Int)
    whenScenarioIs lostQuantumId $ setStore houndsDefeatedKey ([] :: [CardId])
    whenScenarioIs strangeMoonsId $ setStore brainDamagedKey False

  {- End of campaign. Every losing ending calls gameOver from its own
  resolution and never reaches the epilogue, so arriving here is both
  "completed" and "won". -}
  CampaignStep EpilogueStep -> do
    g <- getGame
    let difficulty = campaignDifficulty . toAttrs <$> currentCampaign (gameMode g)

    {- "The Heir to Carcosa": Starfall's Resolution 2 is the only ending that
    follows Tassilda being defeated, and the only place this record is written.
    Defeating her is not enough on its own -- the act can advance into the
    losing agenda branch instead. -}
    whenM (getHasRecord TheInvestigatorsEscapedHastursGrasp) do
      when (difficulty == Just Expert) $ earn TheHeirToCarcosa

    -- "It's Too Late": the campaign-wide Impending Doom tally.
    whenM ((>= 12) <$> getRecordCount ImpendingDoom) $ earn ItsTooLate

    -- "Line in the Sky": win with at least 3 Ultimatums active.
    let ultimatums = length [u | Ultimatum u <- toList $ activeUltimatumsAndBoons (gameSettings g)]
    when (ultimatums >= 3) $ earn LineInTheSky

    {- "Endtimes": the bag as it stands at the end of the campaign. Read from
    the campaign, not the scenario -- there is no scenario left by now. -}
    elderThings <- bagCount ElderThing
    tablets <- bagCount Tablet
    elderSigns <- bagCount ElderSign
    when (elderThings >= 4 && tablets >= 3 && elderSigns == 0) $ earn Endtimes
  {- Deferred threshold checks: bumpCounter and insertGlobal do their arithmetic
  when the message is processed, so the stored value only reads back correctly on
  the trailing Do. -}
  CounterBumped k | k == airlockDefeatsKey -> do
    whenM ((>= 3) <$> storedInt k) $ earn AirlockSequence
  GlobalInserted k | k == houndsDefeatedKey -> do
    hounds <- storedCardIds k
    when (length hounds >= 3) $ earn OutOfTime
  _ -> pure ()

ach :: DarkMatterAchievement -> Achievement
ach = homebrewAchievement achievementCampaign . tshow

earn :: (HasGame m, HasQueue Message m) => DarkMatterAchievement -> m ()
earn = earnAchievement . ach

{- | Gate the whole module (including store writes) to campaigns that can earn
these achievements. Derived from 'achievementCampaigns' so this cannot drift
from 'earnAchievement''s own campaign gate.
-}
whenEligibleCampaign :: HasGame m => m () -> m ()
whenEligibleCampaign body = do
  mCampaignId <- currentCampaignId
  when (maybe False (`elem` achievementCampaigns (ach Untattered)) mCampaignId) body

whenScenarioIs :: HasGame m => ScenarioId -> m () -> m ()
whenScenarioIs sid body = do
  mSid <- selectOne TheScenario
  when (mSid == Just sid) body

theTatterdemalionId, electricNightmareId, lostQuantumId :: ScenarioId
theTatterdemalionId = ":dark-matter:014"
electricNightmareId = ":dark-matter:054"
lostQuantumId = ":dark-matter:089"

strangeMoonsId, theMachineInYellowId :: ScenarioId
strangeMoonsId = ":dark-matter:153"
theMachineInYellowId = ":dark-matter:190"

{- | Strictly "an ability on this location", unlike @source.location@, which also
unwraps a bare location source.
-}
abilityLocation :: Source -> Maybe LocationId
abilityLocation = \case
  AbilitySource s _ -> s.location
  UseAbilitySource _ s _ -> s.location
  PaymentSource s -> abilityLocation s
  IndexedSource _ s -> abilityLocation s
  ProxySource s _ -> abilityLocation s
  _ -> Nothing

-- Campaign store plumbing, mirroring the official campaigns' detection modules.

brainDamagedKey, selfHelpCluesKey, airlockDefeatsKey, cluesSpentKey, houndsDefeatedKey :: Text
brainDamagedKey = "dmAchBrainDamaged"
selfHelpCluesKey = "dmAchSelfHelpClues"
airlockDefeatsKey = "dmAchAirlockDefeats"
cluesSpentKey = "dmAchCluesSpent"
houndsDefeatedKey = "dmAchHoundsDefeated"

-- Priority so the write lands before the rest of the triggering message's
-- cascade, some of which clearQueue.
setStore :: (HasQueue Message m, ToJSON a) => Text -> a -> m ()
setStore k v = push $ Priority $ SetGlobal CampaignTarget (Key.fromText k) (toJSON v)

storedFlag :: (HasCallStack, HasGame m) => Text -> m Bool
storedFlag k = fromMaybe False <$> stored k

storedInt :: (HasCallStack, HasGame m) => Text -> m Int
storedInt k = fromMaybe 0 <$> stored k

storedCardIds :: (HasCallStack, HasGame m) => Text -> m [CardId]
storedCardIds k = fromMaybe [] <$> stored k

-- | How many of a face the campaign's chaos bag holds; 0 when there is no campaign.
bagCount :: HasGame m => ChaosTokenFace -> m Int
bagCount face =
  selectOne TheCampaign >>= maybe (pure 0) (fieldMap CampaignChaosBag (count (== face)))

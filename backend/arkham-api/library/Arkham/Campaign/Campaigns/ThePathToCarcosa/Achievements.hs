{- | Return to The Path to Carcosa achievement detection. Hooked from the
campaign's runMessage (campaign dispatch runs for every message, BEFORE the
scenario and other entities, so defeated enemies etc. are still queryable).
'earnAchievement' self-gates on the achievements setting and on the campaign
id ("52"), so earns stay unconditional here.

Campaign-persistent trackers live in the campaign store (via 'SetGlobal'
messages, read back with 'stored'); the whole module is additionally gated to
achievement-eligible campaigns so base Path to Carcosa games don't accumulate
tracker keys in their store.

Every achievement in this list is only earnable in Return to The Path to
Carcosa, so several of them observe cards ("Host of Insanity", the Lunatic
Dianne Devine, the Secret Passage) that only exist in the Return-to setup.

"Say My Name" (speak the name of Hastur aloud seven or more times) is tracked
from the scenario-reference helper button that applies the warning's horror.
-}
module Arkham.Campaign.Campaigns.ThePathToCarcosa.Achievements (
  runCarcosaAchievements,
) where

import Arkham.Achievement
import Arkham.Agenda.Sequence (agendaSequenceStep)
import Arkham.Agenda.Types (Field (AgendaDeckId, AgendaSequence))
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Types (campaignDifficulty)
import Arkham.CampaignLogKey
import Arkham.Campaigns.ThePathToCarcosa.Key
import Arkham.Card (CardCode, CardDef, toCardCode, toCardDef)
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
import Arkham.Helpers.Log (getHasRecord, getRecordCount, getRecordSet)
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source (Source (..))
import Arkham.Target
import Arkham.Token (Token (..))
import Arkham.Tracing
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.UltimatumsAndBoons.Types
import Data.Aeson.Key qualified as Key

runCarcosaAchievements
  :: (HasGame m, HasQueue Message m, Tracing m) => Message -> m ()
runCarcosaAchievements msg = whenEligibleCampaign $ case msg of
  -- "Say My Name": speak HASTUR aloud seven or more times after heeding
  -- Daniel's warning. The UI button records this by placing the warning's
  -- horror from CampaignSource; keep the count in achievement-only store, not
  -- the public campaign log.
  PlaceTokens CampaignSource (InvestigatorTarget _) Horror n | n > 0 -> do
    whenM (getHasRecord YouHeadedDanielsWarning) do
      c <- storedInt spokenHasturKey
      setStore spokenHasturKey (c + n)
      when (c + n >= 7) do
        earn SayMyName

  -- "First Steps": each VIP interviewed in The Last King checks off their
  -- box (the resolution inserts every interviewed VIP's asset card code into
  -- VIPsInterviewed in a single write); the API layer accumulates the
  -- checklist per user ACROSS playthroughs and awards the earn at five.
  RecordSetInsert key entries | key == toCampaignLogKey VIPsInterviewed -> do
    let interviewed = recordedCardCodes entries
    achievementProgress
      (ThePathToCarcosaAchievement FirstSteps)
      [item | (code, item) <- vipItems, code `elem` interviewed]
  -- Idempotent sweep at every campaign step transition: derive the VIP
  -- checklist from the campaign log, so campaigns whose The Last King ran
  -- before this detection existed still report their progress (the API layer
  -- merge makes re-reports no-ops).
  CampaignStep _ -> do
    interviewed <- recordedCardCodes <$> getRecordSet VIPsInterviewed
    achievementProgress
      (ThePathToCarcosaAchievement FirstSteps)
      [item | (code, item) <- vipItems, code `elem` interviewed]

  -- "The Cuckoo's Nest": resign with Daniel Chesterfield under an
  -- investigator's control. A controlled asset that resigns with its owner
  -- pushes ResignWith for itself.
  ResignWith (AssetTarget aid) -> do
    whenM (selectAny $ AssetWithId aid <> assetIs Assets.danielChesterfield) do
      earn TheCuckoosNest

  -- Ability-1 triggers used by two achievements, distinguished by the ability
  -- source: parleying with the Host of Insanity, or the Secret Passage's
  -- Clasp of Black Onyx shortcut. Both are printed ability 1 on their card.
  UseCardAbility _ source 1 _ _ -> do
    -- "Take a Look at This!": parley with the Host of Insanity (its only
    -- ability requires you to control the Clasp of Black Onyx).
    for_ source.enemy \eid -> do
      whenM (selectAny $ EnemyWithId eid <> enemyIs Enemies.hostOfInsanity) do
        earn TakeALookAtThis
    -- "The Path of Death": use the Clasp of Black Onyx to find a shortcut. The
    -- shortcut lives on the Return-to Secret Passage location.
    for_ source.location \lid -> do
      whenM (selectAny $ LocationWithId lid <> locationIs Locations.returnToSecretPassage) do
        earn ThePathOfDeath

  -- "Guessing Game": race one agenda deck to its final (3rd) agenda before the
  -- other deck's first agenda has advanced. Black Stars Rise runs two 3-stage
  -- agenda decks; advancing a stage-2 agenda is advancing to a deck's final.
  AdvanceAgenda aid -> whenBlackStarsRise do
    deckId <- field AgendaDeckId aid
    step <- agendaSequenceStep <$> field AgendaSequence aid
    when (step == 2) do
      let otherDeck = if deckId == 1 then 2 else 1
      selectOne (AgendaWithDeckId otherDeck) >>= traverse_ \other -> do
        otherStep <- agendaSequenceStep <$> field AgendaSequence other
        when (otherStep == 1) do
          earn GuessingGame

  -- Enemy defeats. The campaign sees Defeated before the enemy processes it,
  -- so the entity is still in play and queryable.
  Defeated (EnemyTarget eid) _ _ _ -> do
    cardDef <- fieldMap EnemyCard toCardDef eid

    -- "Fair Warning": defeat the Royal Emissary three times during Curtain
    -- Call. The Third Act keeps respawning it, so the defeats accumulate.
    when (cardDef == Enemies.royalEmissary) $ whenCurtainCall do
      n <- storedInt royalEmissaryDefeatsKey
      setStore royalEmissaryDefeatsKey (n + 1)
      when (n + 1 >= 3) do
        earn FairWarning

    -- "Crashing the Party": defeat the Lunatic version of Dianne Devine (the
    -- Return-to enemy that carries the Lunatic trait) in The Last King.
    when (cardDef == Enemies.dianneDevineKnowsWhatYoureUpTo) do
      earn CrashingTheParty

    -- "Hastur Made Me Do It": defeat Hastur (any form) in Dim Carcosa while a
    -- Possession treachery is in a hand. Return to Dim Carcosa (the only setup
    -- that can earn this) swaps the base Possession weaknesses for the "Visions
    -- in Your Mind" cards, so both card sets count.
    when (cardDef `elem` hasturForms) do
      whenM (selectAny $ InvestigatorWithTreacheryInHand $ mapOneOf treacheryIs possessionTreacheries) do
        earn HasturMadeMeDoIt

  -- Campaign win: Dim Carcosa's Resolutions 1/2/3 (the surviving wins) all
  -- write TheInvestigatorsPreventedHasturFromEscapingHisPrison. We key win
  -- detection off that record rather than ScenarioResolution.
  Record key | key == toCampaignLogKey TheInvestigatorsPreventedHasturFromEscapingHisPrison -> do
    -- "Get Back Here": win having never ended a scenario with The Man in the
    -- Pallid Mask in play. The disqualify flag is set below at every scenario
    -- resolution; also check him directly here for the final scenario.
    manInPlay <- selectAny $ enemyIs Enemies.theManInThePallidMask
    endedWithMan <- storedFlag endedWithManInPlayKey
    unless (endedWithMan || manInPlay) do
      earn GetBackHere

    doubt <- getRecordCount Doubt
    conviction <- getRecordCount Conviction
    -- "The Path is False" / "The Path is Real" / "The Path is Mine".
    when (doubt == 8) $ earn ThePathIsFalse
    when (conviction == 8) $ earn ThePathIsReal
    when (doubt + conviction < 2) $ earn ThePathIsMine

    g <- getGame
    -- "Line in the Sand": win with at least 3 Ultimatums active.
    let ultimatums = length [u | Ultimatum u <- toList $ activeUltimatumsAndBoons (gameSettings g)]
    when (ultimatums >= 3) $ earn CarcosaLineInTheSand
    -- "Carcosa Expertise": win on Expert.
    let mDifficulty = campaignDifficulty . toAttrs <$> currentCampaign (gameMode g)
    when (mDifficulty == Just Expert) $ earn CarcosaExpertise

  -- "Get Back Here" bookkeeping. Every Carcosa scenario writes its narrative
  -- outcome via a plain Record at resolution — post-clear, with entities
  -- still in play, and with no mid-scenario plain Record anywhere in the
  -- campaign — so this is a reliable "a scenario is resolving" signal. If The
  -- Man is on the board at that moment, the achievement is out for the run.
  Record _ -> whenScenarioActive do
    whenM (selectAny $ enemyIs Enemies.theManInThePallidMask) do
      setStore endedWithManInPlayKey True
    -- "For Prying Eyes": clear every clue from the Hidden Library in Echoes of
    -- the Past so it scores its victory points. The location is never physically
    -- moved to the victory display; a revealed location with no clues counts for
    -- VP at scenario end (see 'getInitialVictory'). Checked at resolution, when
    -- every Carcosa scenario writes a plain Record with entities still in play.
    whenEchoesOfThePast do
      whenM (selectAny $ locationIs Locations.hiddenLibrary <> RevealedLocation <> LocationWithoutClues) do
        earn ForPryingEyes
  _ -> pure ()

earn :: (HasGame m, HasQueue Message m) => ThePathToCarcosaAchievement -> m ()
earn = earnAchievement . ThePathToCarcosaAchievement

{- | Gate the whole module (including store writes) to campaigns that can earn
these achievements. Derived from 'achievementCampaigns' so this cannot drift
from 'earnAchievement''s own campaign gate.
-}
whenEligibleCampaign :: HasGame m => m () -> m ()
whenEligibleCampaign body = do
  mCampaignId <- currentCampaignId
  let eligible = achievementCampaigns $ ThePathToCarcosaAchievement GetBackHere
  when (maybe False (`elem` eligible) mCampaignId) body

whenScenarioActive :: (HasGame m, Tracing m) => m () -> m ()
whenScenarioActive body = whenM (isJust <$> selectOne TheScenario) body

whenScenarioIn :: (HasGame m, Tracing m) => [ScenarioId] -> m () -> m ()
whenScenarioIn sids body = do
  mSid <- selectOne TheScenario
  when (maybe False (`elem` sids) mSid) body

whenCurtainCall :: (HasGame m, Tracing m) => m () -> m ()
whenCurtainCall = whenScenarioIn ["03043", "52014"]

whenEchoesOfThePast :: (HasGame m, Tracing m) => m () -> m ()
whenEchoesOfThePast = whenScenarioIn ["03120", "52028"]

whenBlackStarsRise :: (HasGame m, Tracing m) => m () -> m ()
whenBlackStarsRise = whenScenarioIn ["03274", "52054"]

-- VIP bystander asset card code (as recorded in VIPsInterviewed) ->
-- checklist item key from 'achievementChecklist'.
vipItems :: [(CardCode, Text)]
vipItems =
  [ (toCardCode Assets.constanceDumaine, "ConstanceDumaine")
  , (toCardCode Assets.sebastienMoreau, "SebastienMoreau")
  , (toCardCode Assets.jordanPerry, "JordanPerry")
  , (toCardCode Assets.ashleighClarke, "AshleighClarke")
  , (toCardCode Assets.ishimaruHaruko, "IshimaruHaruko")
  ]

hasturForms :: [CardDef]
hasturForms =
  [ Enemies.hasturTheKingInYellow
  , Enemies.hasturLordOfCarcosa
  , Enemies.hasturTheTatteredKing
  ]

possessionTreacheries :: [CardDef]
possessionTreacheries =
  [ Treacheries.possessionTraitorous
  , Treacheries.possessionTorturous
  , Treacheries.possessionMurderous
  ]

-- Campaign store plumbing. Writes go through the queue ('SetGlobal' is handled
-- by the campaign runner); reads see all previously processed writes.

royalEmissaryDefeatsKey, endedWithManInPlayKey, spokenHasturKey :: Text
royalEmissaryDefeatsKey = "carcosaAchRoyalEmissaryDefeats"
endedWithManInPlayKey = "carcosaAchEndedWithManInPlay"
spokenHasturKey = "carcosaAchSpokenHastur"

-- Priority so the write is applied before the rest of the triggering
-- message's cascade — some cascades (e.g. defeating a victory enemy)
-- clearQueue, which would otherwise drop a plainly-pushed store write.
setStore :: (HasQueue Message m, ToJSON a) => Text -> a -> m ()
setStore k v = push $ Priority $ SetGlobal CampaignTarget (Key.fromText k) (toJSON v)

storedInt :: (HasCallStack, HasGame m, Tracing m) => Text -> m Int
storedInt k = fromMaybe 0 <$> stored k

storedFlag :: (HasCallStack, HasGame m, Tracing m) => Text -> m Bool
storedFlag k = fromMaybe False <$> stored k

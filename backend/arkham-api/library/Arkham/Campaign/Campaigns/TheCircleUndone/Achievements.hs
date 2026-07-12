{- | Return to The Circle Undone achievement detection. Hooked from the
campaign's runMessage (campaign dispatch runs for every message, BEFORE the
scenario and other entities, so defeated enemies etc. are still queryable).
'earnAchievement' self-gates on the achievements setting and on the campaign
id ("54"), so earns stay unconditional here.

Campaign-persistent trackers live in the campaign store (via 'SetGlobal'
messages, read back with 'stored'); the whole module is additionally gated to
achievement-eligible campaigns so base Circle Undone games don't accumulate
tracker keys in their store.

The four surviving endings of Before the Black Throne all write
'AzathothSlumbersForNow', but three of them (Pipers/Signed/Reversed) write a
unique key FIRST, so "slumbers with none of those" is exactly Resolution 3,
the "craft a spell to contain Azathoth" ending (Weaver of Shadow and Mist).
-}
module Arkham.Campaign.Campaigns.TheCircleUndone.Achievements (
  runCircleAchievements,
) where

import Arkham.Achievement
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Types (campaignDifficulty)
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Card (cardMatch, toCardDef)
import Arkham.Card.CardDef (cdCardTraits)
import Arkham.Classes.Entity (toAttrs)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Difficulty
import Arkham.Enemy.Types (Field (EnemyTraits))
import Arkham.Game.Base
import Arkham.Helpers.Campaign (stored)
import Arkham.Helpers.Log (getHasRecord, getRecordSet)
import Arkham.Helpers.Scenario (scenarioFieldMap)
import Arkham.Id
import Arkham.Matcher hiding (Discarded)
import Arkham.Message
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (ScenarioVictoryDisplay))
import Arkham.Target
import Arkham.Tracing
import Arkham.Trait (Trait (Geist, SilverTwilight, Spectral))
import Data.Aeson.Key qualified as Key

runCircleAchievements
  :: (HasGame m, HasQueue Message m, Tracing m) => Message -> m ()
runCircleAchievements msg = whenEligibleCampaign $ case msg of
  -- "Savior of Humanity": rescue every Silver Twilight enemy in At Death's
  -- Doorstep. Escaping the Spectral Realm (Resolution 1) is the only surviving
  -- win; "rescued every one" = none was killed, i.e. no Silver Twilight card
  -- in the victory display when that record is written (board state is still
  -- intact — the record is pushed before endOfScenario).
  Record key | key == toCampaignLogKey TheInvestigatorsEscapedTheSpectralRealm -> do
    silverInVictory <-
      scenarioFieldMap ScenarioVictoryDisplay (count (`cardMatch` CardWithTrait SilverTwilight))
    when (silverInVictory == 0) $ earn SaviorOfHumanity

  -- "Carl Shmarl": inducted into the Inner Circle (interlude 3), then side
  -- with the Coven in Union and Disillusion (recorded at its PreScenarioSetup).
  Record key | key == toCampaignLogKey TheInvestigatorsSidedWithTheCoven -> do
    whenM (getHasRecord TheInvestigatorsWereInductedIntoTheInnerCircle) $ earn CarlShmarl

  -- "The Threefold Rule": join forces with Erynn (The Wages of Sin), then side
  -- with the Lodge in Union and Disillusion, turning on her coven.
  Record key | key == toCampaignLogKey TheInvestigatorsSidedWithTheLodge -> do
    whenM (getHasRecord ErynnJoinedTheInvestigators) $ earn TheThreefoldRule

  -- "New World Order" / "Immortality Sounds Nice": the campaign ends in the
  -- Lodge's / Coven's favour (Union and Disillusion Resolutions 2 / 9, each a
  -- distinct gameOver record).
  Record key
    | key == toCampaignLogKey TheTrueWorkOfTheSilverTwilightLodgeHasBegun ->
        earn NewWorldOrder
  Record key
    | key == toCampaignLogKey TheCovenOfKeziahHoldsTheWorldInItsGrasp ->
        earn ImmortalitySoundsNice
  -- The four surviving endings of Before the Black Throne.
  Record key
    | key == toCampaignLogKey TheLeadInvestigatorHasJoinedThePipersOfAzathoth ->
        earn MusicOfTheOuterGods
  Record key
    | key == toCampaignLogKey TheInvestigatorsSignedTheBlackBookOfAzathoth ->
        earn FinePrint
  Record key
    | key == toCampaignLogKey TheInvestigatorsReversedTheIncantation ->
        earn SpeakTheWordsAloud
  Record key | key == toCampaignLogKey AzathothSlumbersForNow -> do
    -- "Weaver of Shadow and Mist": Resolution 3 records only slumbers; the
    -- other three wins record their unique key before slumbers, so their
    -- absence here pins this to the "contain Azathoth" ending.
    otherEnding <-
      or
        <$> traverse
          getHasRecord
          [ TheLeadInvestigatorHasJoinedThePipersOfAzathoth
          , TheInvestigatorsSignedTheBlackBookOfAzathoth
          , TheInvestigatorsReversedTheIncantation
          ]
    unless otherEnding $ earn WeaverOfShadowAndMist
    -- "Circle Expertise": win the campaign (any surviving ending) on Expert.
    g <- getGame
    when (fmap (campaignDifficulty . toAttrs) (currentCampaign (gameMode g)) == Just Expert) do
      earn CircleExpertise

  -- "Case Closed": save each of the four missing people. Their *Fate stories
  -- record <name>IsAlive; the API accumulates the checklist across playthroughs
  -- and awards the earn once all four are checked.
  Record key | key == toCampaignLogKey ValentinoIsAlive -> progressCase "ValentinoRivas"
  Record key | key == toCampaignLogKey GavriellaIsAlive -> progressCase "GavriellaMizrah"
  Record key | key == toCampaignLogKey PennyIsAlive -> progressCase "PennyWhite"
  Record key | key == toCampaignLogKey JeromeIsAlive -> progressCase "JeromeDavids"
  -- "More Like Excursion": complete In the Clutches of Chaos without a single
  -- incursion. Any incursion routes through the 'Incursion' message; the two
  -- surviving resolutions each write one of these records.
  Incursion _ -> setStore incursionOccurredKey True
  Record key | key `elem` map toCampaignLogKey clutchesWinRecords -> whenInTheClutchesOfChaos do
    occurred <- storedFlag incursionOccurredKey
    unless occurred $ earn MoreLikeExcursion

  -- "'Member These?": discover all ten Mementos (checklist, accumulated by the
  -- API across playthroughs). Read the newly inserted mementos from the write.
  RecordSetInsert key entries | key == toCampaignLogKey MementosDiscovered -> do
    let discovered = mapMaybe (unrecorded @Memento) entries
    achievementProgress (TheCircleUndoneAchievement MemberThese) (map mementoItem discovered)

  -- Idempotent sweep at every campaign step transition: derive both checklists
  -- from the campaign log, so campaigns whose records predate this detection
  -- still report their progress (the API layer merge makes re-reports no-ops).
  CampaignStep _ -> do
    alive <- filterM (getHasRecord . fst) caseClosedItems
    achievementProgress (TheCircleUndoneAchievement CaseClosed) (map snd alive)
    discovered <- mapMaybe (unrecorded @Memento) <$> getRecordSet MementosDiscovered
    achievementProgress (TheCircleUndoneAchievement MemberThese) (map mementoItem discovered)

  -- "Who You Gonna Call?": defeat 13 Geist or Spectral enemies that are
  -- discarded or added to the victory display. Enemies that return to
  -- set-aside/void have their Discard/AddToVictory cancelled out of the queue,
  -- so counting at those two messages excludes them automatically.
  AddToVictory _ (EnemyTarget eid) -> do
    traits <- field EnemyTraits eid
    when (isGeistOrSpectral traits) bumpGeist
  Discarded (EnemyTarget _) _ card -> do
    when (isGeistOrSpectral $ cdCardTraits $ toCardDef card) bumpGeist

  -- "10/10 Would Read Again": take 10 horror total via The Black Book's ability
  -- (its horror is a HorrorCostX cost paid to the asset's ability source, which
  -- 'src.asset' unwraps through the PaymentSource/AbilitySource wrappers).
  InvestigatorAssignDamage _ src _ _ horror | horror > 0 -> do
    for_ src.asset \aid -> do
      whenM (selectAny $ AssetWithId aid <> assetIs Assets.theBlackBook) do
        c <- storedInt blackBookHorrorKey
        setStore blackBookHorrorKey (c + horror)
        when (c + horror >= 10) $ earn TenOutOfTenWouldReadAgain
  _ -> pure ()

earn :: (HasGame m, HasQueue Message m) => TheCircleUndoneAchievement -> m ()
earn = earnAchievement . TheCircleUndoneAchievement

progressCase :: (HasGame m, HasQueue Message m) => Text -> m ()
progressCase item = achievementProgress (TheCircleUndoneAchievement CaseClosed) [item]

bumpGeist :: (HasCallStack, HasGame m, HasQueue Message m, Tracing m) => m ()
bumpGeist = do
  n <- storedInt geistDefeatsKey
  setStore geistDefeatsKey (n + 1)
  when (n + 1 >= 13) $ earn WhoYouGonnaCall

isGeistOrSpectral :: Set Trait -> Bool
isGeistOrSpectral traits = Geist `member` traits || Spectral `member` traits

{- | Gate the whole module (including store writes) to campaigns that can earn
these achievements. Derived from 'achievementCampaigns' so this cannot drift
from 'earnAchievement''s own campaign gate.
-}
whenEligibleCampaign :: HasGame m => m () -> m ()
whenEligibleCampaign body = do
  mCampaignId <- currentCampaignId
  let eligible = achievementCampaigns $ TheCircleUndoneAchievement CircleExpertise
  when (maybe False (`elem` eligible) mCampaignId) body

whenInTheClutchesOfChaos :: (HasGame m, Tracing m) => m () -> m ()
whenInTheClutchesOfChaos body = do
  mSid <- selectOne TheScenario
  when (maybe False (`elem` (["05284", "54049"] :: [ScenarioId])) mSid) body

-- The seven records that a surviving Resolution 1/2 of In the Clutches of Chaos
-- can write (exactly one fires per win). Resolutions 3/4 write DoomDrawsEverCloser.
clutchesWinRecords :: [TheCircleUndoneKey]
clutchesWinRecords =
  [ TheInvestigatorsContinuedAlone
  , TheInvestigatorsAskedAnetteForAssistance
  , TheInvestigatorsArrestedAnette
  , AnetteTaughtYouTheSpellsOfOld
  , TheInvestigatorsAskedSanfordForAssistance
  , TheInvestigatorsArrestedSanford
  , TheInvestigatorsAssumedControlOfTheSilverTwilightLodge
  ]

-- <name>IsAlive key -> checklist item key (mirrors 'achievementChecklist'
-- CaseClosed).
caseClosedItems :: [(TheCircleUndoneKey, Text)]
caseClosedItems =
  [ (ValentinoIsAlive, "ValentinoRivas")
  , (GavriellaIsAlive, "GavriellaMizrah")
  , (PennyIsAlive, "PennyWhite")
  , (JeromeIsAlive, "JeromeDavids")
  ]

-- Memento -> checklist item key (mirrors 'achievementChecklist' MemberThese).
mementoItem :: Memento -> Text
mementoItem = \case
  MesmerizingFlute -> "MesmerizingFlute"
  RitualComponents -> "RitualComponents"
  ScrapOfTornShadow -> "ScrapOfTornShadow"
  StrangeIncantation -> "StrangeIncantation"
  Gilman'sJournal -> "GilmansJournal"
  Keziah'sFormulae -> "KeziahsFormulae"
  WornCrucifix -> "WornCrucifix"
  WispOfSpectralMist -> "WispOfSpectralMist"
  CornHuskDoll -> "CornHuskDoll"
  BloodyTreeCarvings -> "BloodyTreeCarvings"

-- Campaign store plumbing. Writes go through the queue ('SetGlobal' is handled
-- by the campaign runner); reads see all previously processed writes.

geistDefeatsKey, blackBookHorrorKey, incursionOccurredKey :: Text
geistDefeatsKey = "circleAchGeistDefeats"
blackBookHorrorKey = "circleAchBlackBookHorror"
incursionOccurredKey = "circleAchIncursionOccurred"

-- Priority so the write is applied before the rest of the triggering message's
-- cascade — some cascades (e.g. defeating a victory enemy) clearQueue, which
-- would otherwise drop a plainly-pushed store write.
setStore :: (HasQueue Message m, ToJSON a) => Text -> a -> m ()
setStore k v = push $ Priority $ SetGlobal CampaignTarget (Key.fromText k) (toJSON v)

storedInt :: (HasCallStack, HasGame m, Tracing m) => Text -> m Int
storedInt k = fromMaybe 0 <$> stored k

storedFlag :: (HasCallStack, HasGame m, Tracing m) => Text -> m Bool
storedFlag k = fromMaybe False <$> stored k

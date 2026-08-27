{- | Children of Blood achievement detection. Hooked from the campaign's
runMessage, which dispatches before the scenario and other entities, so defeated
enemies are still queryable at 'Defeated' time.

Scenario-end detections key on 'EndOfGame' rather than 'ScenarioResolution': the
Scenario wrapper clearQueues twice while processing a resolution, wiping even
Priority pushes made during that dispatch. 'EndOfGame' is pushed from the
resolution body afterwards, with the board still intact.
-}
module Arkham.Campaign.Campaigns.ChildrenOfBlood.Achievements (
  runChildrenOfBloodAchievements,
) where

import Arkham.Achievement
import Arkham.Agenda.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Agendas
import Arkham.Agenda.Types (Field (AgendaCard))
import Arkham.Asset.Cards.ChildrenOfBlood qualified as Assets
import Arkham.Campaign.Types (campaignChaosBag, campaignDifficulty)
import Arkham.CampaignStep
import Arkham.Card
import Arkham.ChaosToken.Types
import Arkham.Classes.Entity (toAttrs)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Enemy.Types (Field (EnemyCard))
import Arkham.Game.Base
import Arkham.Helpers.Campaign (getCampaignStoryCards, stored)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Helpers.Scenario (getVictoryDisplay)
import Arkham.Id
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.CardDefs.ChildrenOfBlood.BloodMoney qualified as Locations
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Locations
import Arkham.Location.Types (Field (LocationCard))
import Arkham.Matcher hiding (LocationCard)
import Arkham.Message
import Arkham.Name (toTitle)
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Trait (Trait (Civilian))
import Arkham.Treachery.CardDefs.ChildrenOfBlood.Infected qualified as Treacheries
import Data.Aeson.Key qualified as Key

runChildrenOfBloodAchievements
  :: (HasGame m, HasQueue Message m) => Message -> m ()
runChildrenOfBloodAchievements msg = whenEligibleCampaign $ case msg of
  -- "Hide and Seek": agenda 2a in River of Blood is The First Night; once it has
  -- advanced, Julia can no longer be defeated "before" it.
  AdvanceAgenda aid -> whenScenarioIs riverOfBloodId do
    cardDef <- fieldMap AgendaCard toCardDef aid
    when (cardDef == Agendas.theFirstNight) $ setStore agenda2AdvancedKey True
  Defeated (EnemyTarget eid) _ source _ -> do
    cardDef <- fieldMap EnemyCard toCardDef eid
    let title = toTitle cardDef.name

    -- Any Civilian defeat disqualifies that scenario's "no civilians" achievement.
    when (Civilian `member` cdCardTraits cardDef) $ setStore civilianDefeatedKey True

    whenScenarioIs riverOfBloodId
      $ when (title == "Julia Stern")
      $ unlessM (storedFlag agenda2AdvancedKey)
      $ earn HideAndSeek

    whenScenarioIs newHorizonsId $ when (title == "Zburamoarte") do
      viaLair <- isLocationAbility Locations.descendingTunnel 2 source
      when viaLair $ earn ThatLlLearnHim

    whenScenarioIs bloodMoneyId $ when (title == "Howard Wilkes") do
      -- "Hell Hath No Fury": Julia's Forced ability damages Wilkes with herself
      -- as the source, so the killing source unwraps to a Julia Stern enemy.
      juliaKilled <- sourceIsEnemyNamed "Julia Stern" source
      when juliaKilled $ earn HellHathNoFury
      viaBalcony <- isLocationAbility Locations.balcony 1 source
      when viaBalcony $ earn LookOutBelow

  -- Story assets and rewards reach a deck through the campaign's card list.
  AddCampaignCardToDeck _ _ card -> do
    let code = toCardCode card
    when (code == toCardCode Assets.fangOfZburamoarte) $ earn TrophyCollector
    when (code `elem` [toCardCode Assets.forgedPermit, toCardCode Assets.sanguineSong])
      $ earn FilingPaperwork
    when (code == toCardCode Assets.charlieKaneKnowsAGuy) $ earn ThanksForYourVote

  -- "You Got Red on You": the campaign caps sealed {blood} at 3 per investigator,
  -- so this is hitting the cap without any of them carrying over from an earlier
  -- round. The per-round tally resets at BeginRound.
  SealedChaosToken token (Just iid) target | token.face == #blood -> do
    when (target == InvestigatorTarget iid) do
      sealedThisRound <- storedList sealedThisRoundKey
      let sealedThisRound' = sealedThisRound <> [iid]
      setStore sealedThisRoundKey sealedThisRound'
      -- credited to that investigator's player only, as the card is worded
      when (count (== iid) sealedThisRound' >= 3) $ earnFor iid YouGotRedOnYou
  BeginRound -> setStore sealedThisRoundKey ([] :: [InvestigatorId])
  -- Per-scenario trackers start clean.
  CampaignStep (ScenarioStep _) -> setStore civilianDefeatedKey False
  EndOfGame _ -> do
    noCivilianDefeats <- not <$> storedFlag civilianDefeatedKey

    whenScenarioIs riverOfBloodId $ when noCivilianDefeats $ earn FriendlyNeighborhoodInvestigator

    whenScenarioIs newHorizonsId do
      when noCivilianDefeats $ earn SafetyInspector
      -- "No Meat-Slab Unhooked": every location revealed, no clues left on any.
      allRevealed <- selectNone UnrevealedLocation
      noClues <- selectNone LocationWithAnyClues
      when (allRevealed && noClues) $ earn NoMeatSlabUnhooked

    whenScenarioIs bloodMoneyId do
      when noCivilianDefeats $ earn APartyEveryoneCanEnjoy
      -- "If You Can't Beat Them...": every available Suspicious Guest ended in
      -- the victory display.
      victory <- getVictoryDisplay
      let inVictory = count ((== "Suspicious Guest") . toTitle . (.name) . toCardDef) victory
      -- 6 copies start the scenario; setup removes 2 at one investigator and 1
      -- at two, so the number still available is a function of player count.
      n <- getPlayerCount
      let available = 6 - (if n == 1 then 2 else if n == 2 then 1 else 0)
      when (inVictory >= available) $ earn IfYouCantBeatThem
      -- "And All I Got Were These Bloody Fangs": R1 and R2 both unlock the reward
      -- card when at least one investigator bears The Blood Blight. A numbered
      -- resolution is what separates them from No Resolution, which is only
      -- reached once every investigator has resigned or been defeated.
      reachedResolution <- selectAny UneliminatedInvestigator
      blighted <- anyBloodBlightBearer
      when (reachedResolution && blighted) $ earn AndAllIGotWereTheseBloodyFangs

  -- Campaign completion: the chaos bag tally and both checklists.
  CampaignStep EpilogueStep -> do
    -- the campaign's bag, not the scenario's: there is no scenario in play at the
    -- epilogue, so a chaos-token query would count nothing
    g <- getGame
    let campaignBag = maybe [] (campaignChaosBag . toAttrs) (currentCampaign (gameMode g))
    let bloods = count (== BloodToken) campaignBag
    when (bloods == 0) $ earn DontForgetYourPPE
    when (bloods >= 12) $ earn BathedInBlood

    -- credited to the player who actually played that investigator
    for_ backstoryItems \(def, key) ->
      selectOne (IncludeEliminated $ investigatorIs def) >>= traverse_ \iid ->
        achievementProgressBy iid (ChildrenOfBloodAchievement ItsPartOfMyBackstory) [key]

    -- the campaign's difficulty, not getDifficulty: there is no scenario in play
    -- at the epilogue for a scenario field to read from
    let difficulty = campaignDifficulty . toAttrs <$> currentCampaign (gameMode g)
    for_ difficulty \d ->
      achievementProgress (ChildrenOfBloodAchievement WaterfrontWetWork) [tshow d]
  _ -> pure ()

-- | Whether any investigator carries The Blood Blight as a campaign story card.
anyBloodBlightBearer :: HasGame m => m Bool
anyBloodBlightBearer = do
  storyCards <- getCampaignStoryCards
  pure $ any (any ((== Treacheries.theBloodBlight) . toCardDef)) (toList storyCards)

-- | The killing source unwraps to ability @n@ on a location with this card def.
isLocationAbility :: HasGame m => CardDef -> Int -> Source -> m Bool
isLocationAbility def n source = case asAbilitySource source of
  AbilitySource (LocationSource lid) n' | n == n' -> fieldMap LocationCard ((== def) . toCardDef) lid
  _ -> pure False

-- | The killing source unwraps to an enemy with this title.
sourceIsEnemyNamed :: HasGame m => Text -> Source -> m Bool
sourceIsEnemyNamed title = \case
  EnemySource eid -> fieldMap EnemyCard ((== title) . toTitle . (.name) . toCardDef) eid
  _ -> pure False

earn :: (HasGame m, HasQueue Message m) => ChildrenOfBloodAchievement -> m ()
earn = earnAchievement . ChildrenOfBloodAchievement

-- | Credit a single investigator's player rather than the whole table.
earnFor :: (HasGame m, HasQueue Message m) => InvestigatorId -> ChildrenOfBloodAchievement -> m ()
earnFor iid = earnAchievementBy iid . ChildrenOfBloodAchievement

whenEligibleCampaign :: HasGame m => m () -> m ()
whenEligibleCampaign body = do
  mCampaignId <- currentCampaignId
  let eligible = achievementCampaigns $ ChildrenOfBloodAchievement HideAndSeek
  when (maybe False (`elem` eligible) mCampaignId) body

whenScenarioIs :: HasGame m => ScenarioId -> m () -> m ()
whenScenarioIs sid body = do
  mSid <- selectOne TheScenario
  when (mSid == Just sid) body

riverOfBloodId, newHorizonsId, bloodMoneyId :: ScenarioId
riverOfBloodId = "13001"
newHorizonsId = "13031"
bloodMoneyId = "13068"

backstoryItems :: [(CardDef, Text)]
backstoryItems =
  [ (Investigators.danielaReyes, "DanielaReyes")
  , (Investigators.danielaReyes2, "DanielaReyes")
  , (Investigators.miguelDeLaCruz, "MigueldelaCruz")
  ]

agenda2AdvancedKey, civilianDefeatedKey, sealedThisRoundKey :: Text
agenda2AdvancedKey = "cob.agenda2Advanced"
civilianDefeatedKey = "cob.civilianDefeated"
sealedThisRoundKey = "cob.sealedThisRound"

setStore :: (HasQueue Message m, ToJSON a) => Text -> a -> m ()
setStore k v = push $ Priority $ SetGlobal CampaignTarget (Key.fromText k) (toJSON v)

storedFlag :: (HasCallStack, HasGame m) => Text -> m Bool
storedFlag k = fromMaybe False <$> stored k

storedList :: (HasCallStack, HasGame m) => Text -> m [InvestigatorId]
storedList k = fromMaybe [] <$> stored k

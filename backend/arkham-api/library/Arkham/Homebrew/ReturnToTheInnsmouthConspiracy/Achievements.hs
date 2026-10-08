{- | (Unofficial) Return to The Innsmouth Conspiracy achievement detection.

Hooked from the box campaign's runMessage, so it sees every message BEFORE the scenario and
the entities: a defeated enemy is still in play, and the chaos bag has not yet taken the
token being added.

'earnAchievement' self-gates on the achievements setting and on the campaign id
(":return-to-the-innsmouth-conspiracy"), so earns here stay unconditional; the module as a
whole is gated by 'whenEligibleCampaign' so its store writes stay out of other campaigns.
The official campaign's own detection runs too -- the box delegates to it -- but its list is
gated to campaign "07", so a Return to game earns only these.

Scenario-end detections key on 'EndOfGame', NOT on 'ScenarioResolution': the Scenario
wrapper clearQueues twice while processing a resolution, wiping even Priority pushes.
-}
module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Achievements (
  runReturnToTheInnsmouthConspiracyAchievements,
) where

import Arkham.Achievement
import Arkham.Asset.Cards.TheInnsmouthConspiracy qualified as Assets
import Arkham.Asset.Types qualified as Asset
import Arkham.Campaign.Types (campaignDifficulty)
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.Card
import Arkham.ChaosToken.Types
import Arkham.Classes.Entity (toAttrs)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Difficulty
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.ALightInTheFog qualified as Enemies
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.CreaturesOfTheDeep qualified as Enemies
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.IntoTheMaelstrom qualified as Enemies
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.TheLairOfDagon qualified as Enemies
import Arkham.Enemy.Types qualified as Enemy
import Arkham.Game.Base
import Arkham.Helpers.Campaign (stored)
import Arkham.Helpers.Log (getRecordSet)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.AchievementDefs
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Assets qualified as HBAssets
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (deepOneInvestigatorsInCampaign)
import Arkham.Id
import Arkham.Investigator.Types qualified as Investigator
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Target
import Arkham.Trait (Trait (DeepOne))
import Data.Aeson.Key qualified as Key

runReturnToTheInnsmouthConspiracyAchievements
  :: (HasGame m, HasQueue Message m) => Message -> m ()
runReturnToTheInnsmouthConspiracyAchievements msg = whenEligibleCampaign $ case msg of
  {- "A Full Bag": ten bless and ten curse tokens in the chaos bag at once -- the whole of
  both pools. The bag has not taken this token yet, so the arriving face counts itself. -}
  AddChaosTokenWith details | details.face `elem` [#bless, #curse] -> do
    blesses <- inBag #bless
    curses <- inBag #curse
    let arriving f = if details.face == f then 1 else 0 :: Int
    when (blesses + arriving #bless >= 10 && curses + arriving #curse >= 10) $ earn AFullBag

    {- Half of "Making Your Own Luck": the curses have to reach ten before they drain. -}
    whenScenarioIs returnToTheLairOfDagonId do
      when (curses + arriving #curse >= 10) $ setStore tenCursesKey True
  -- The other half: all the way back down to none, having been at ten.
  RemoveChaosToken face | face == #curse -> whenScenarioIs returnToTheLairOfDagonId do
    curses <- inBag #curse
    when (curses <= 1) $ whenM (storedFlag tenCursesKey) $ earn MakingYourOwnLuck
  -- Enemy defeats. The campaign sees Defeated before the enemy processes it, and the
  -- message already carries the enemy's traits.
  Defeated (EnemyTarget eid) _ _ traits -> do
    -- "This Guy Again": five Deep One Bulls over the campaign, so no scenario gate.
    def <- fieldMap Enemy.EnemyCard toCardDef eid
    when (isPrintingOfAny [Enemies.deepOneBull] def) $ bumpCounter bullDefeatsKey 1
    -- "Run for Your Lives" bookkeeping: any Deep One defeated disqualifies In Too Deep.
    when (DeepOne `elem` traits)
      $ whenScenarioIs returnToInTooDeepId
      $ setStore deepOneDefeatedKey True
  {- "Way Too Cute": four Deep One Hatchlings in play at once. 'EnemySpawned' is the
  after-message, so the newcomer is already placed. -}
  EnemySpawned details -> whenScenarioIs returnToALightInTheFogId do
    def <- fieldMap Enemy.EnemyCard toCardDef details.enemy
    when (isPrintingOfAny [Enemies.deepOneHatchling] def) do
      hatchlings <- select $ enemyIs Enemies.deepOneHatchling
      when (length (nub $ details.enemy : hatchlings) >= 4) $ earn WayTooCute
  {- "Friend of the People": all three of the box's townspeople at once. Checked as the
  third arrives, since the campaign runs before the asset does and so cannot see the
  newcomer in play yet -- only the two already there. -}
  CardEnteredPlay _ card -> checkFriendOfThePeople (toCardDef card)
  TakeControlOfAsset _ aid -> checkFriendOfThePeople =<< fieldMap Asset.AssetCard toCardDef aid
  {- "Step on It" bookkeeping: ability 1 on a running car is the drive that outruns the
  pursuit. Any round that ends without one breaks the run. -}
  UseCardAbility _ source 1 _ _ | Just aid <- source.asset -> whenScenarioIs returnToHorrorInHighGearId do
    def <- fieldMap Asset.AssetCard toCardDef aid
    when (def `elem` runningCars) $ setStore droveThisRoundKey True
  EndRound -> whenScenarioIs returnToHorrorInHighGearId do
    unlessM (storedFlag droveThisRoundKey) $ setStore stepOnItBrokenKey True
  BeginRound -> whenScenarioIs returnToHorrorInHighGearId $ setStore droveThisRoundKey False
  {- "Good Swimmer" bookkeeping: the vessel moving. The campaign sees this before the
  placement changes, so a different location already there means it is being moved rather
  than placed during setup. -}
  PlaceAsset aid (AtLocation lid) -> whenScenarioIs returnToDevilReefId do
    def <- fieldMap Asset.AssetCard toCardDef aid
    when (def `elem` fishingVessels) do
      placement <- field Asset.AssetPlacement aid
      case placement of
        AtLocation lid' | lid' /= lid -> setStore vesselMovedKey True
        _ -> pure ()
  {- "Go Back to Sleep": both gods exhausted at once during the investigator phase. Sampled
  at turn boundaries -- exhaustion lasts until upkeep readies them, so a turn ending with
  both exhausted is the state the achievement asks for. -}
  EndTurn _ -> checkGodsAsleep
  BeginTurn _ -> checkGodsAsleep
  {- "Actually Just Here for the Money": Agent Harper's mission is the recovery of the
  riches of Y'ha-nthlei, and both its endings record it -- the costly one included. -}
  Record key
    | key
        `elem` map
          toCampaignLogKey
          [AgentHarpersMissionIsComplete, AgentHarpersMissionIsCompleteButAtWhatCost] ->
        earn ActuallyJustHereForTheMoney
  -- Per-game flags reset as their scenario is set up, so a revisited scenario cannot
  -- inherit a previous game's state.
  Setup -> do
    whenScenarioIs returnToInTooDeepId $ setStore deepOneDefeatedKey False
    whenScenarioIs returnToDevilReefId $ setStore vesselMovedKey False
    whenScenarioIs returnToTheLairOfDagonId $ setStore tenCursesKey False
    whenScenarioIs returnToHorrorInHighGearId do
      setStore droveThisRoundKey True
      setStore stepOnItBrokenKey False
  -- Scenario-completion detections. See the module header for why these hang off EndOfGame.
  EndOfGame _ ->
    selectOne TheScenario >>= traverse_ \sid ->
      if
        | sid == returnToThePitOfDespairId -> do
            {- "Off to a Good Start": one stamina and one sanity left, no more and no less.
            Earned for that investigator's player alone. -}
            eachLivingInvestigator \iid -> do
              health <- field Investigator.InvestigatorRemainingHealth iid
              sanity <- field Investigator.InvestigatorRemainingSanity iid
              when (health == 1 && sanity == 1) $ earnBy iid OffToAGoodStart
        | sid == returnToInTooDeepId ->
            -- "Run for Your Lives": nothing with the Deep One trait was defeated.
            unlessM (storedFlag deepOneDefeatedKey) $ earn RunForYourLives
        | sid == returnToDevilReefId -> do
            -- "Good Swimmer": the vessel never moved, and a relic was still claimed.
            moved <- storedFlag vesselMovedKey
            claimed <- selectAny $ mapOneOf assetIs relics
            when (not moved && claimed) $ earn GoodSwimmer
        | sid == returnToHorrorInHighGearId ->
            -- "Step on It": every round was driven.
            unlessM (storedFlag stepOnItBrokenKey) $ earn StepOnIt
        | otherwise -> pure ()
  {- End of campaign. Every surviving ending routes through the epilogue; the endings that
  wipe or doom the party call gameOver instead and never get here, so reaching this step is
  both "completed" and "won". -}
  CampaignStep EpilogueStep -> do
    memories <- getRecordSet MemoriesRecovered
    -- "I Do Not Recall This Place" / "Total Recall": the two ends of the memory tally. The
    -- sixteen are the fourteen printed, Flashback XVI's, and The Horrible Truth.
    when (null memories) $ earn IDoNotRecallThisPlace
    when (length memories >= 16) $ earn TotalRecall

    -- "Something Smells Fishy": won while carrying Innsmouth Influence.
    deepOnes <- deepOneInvestigatorsInCampaign
    unless (null deepOnes) $ earn SomethingSmellsFishy

    -- "Expert Fisherman": won on Expert.
    difficulty <- fmap (campaignDifficulty . toAttrs) . currentCampaign . gameMode <$> getGame
    when (difficulty == Just Expert) $ earn ExpertFisherman
  CounterBumped k
    | k == bullDefeatsKey -> whenM ((>= 5) <$> storedInt k) $ earn ThisGuyAgain
  _ -> pure ()

ach :: ReturnToTheInnsmouthConspiracyAchievement -> Achievement
ach = homebrewAchievement achievementCampaign . tshow

earn :: (HasGame m, HasQueue Message m) => ReturnToTheInnsmouthConspiracyAchievement -> m ()
earn = earnAchievement . ach

earnBy
  :: (HasGame m, HasQueue Message m)
  => InvestigatorId
  -> ReturnToTheInnsmouthConspiracyAchievement
  -> m ()
earnBy iid = earnAchievementBy iid . ach

{- | Gate the whole module (including store writes) to campaigns that can earn these
achievements. Derived from 'achievementCampaigns' so this cannot drift from
'earnAchievement''s own campaign gate.
-}
whenEligibleCampaign :: HasGame m => m () -> m ()
whenEligibleCampaign body = do
  mCampaignId <- currentCampaignId
  when (maybe False (`elem` achievementCampaigns (ach AFullBag)) mCampaignId) body

whenScenarioIs :: HasGame m => ScenarioId -> m () -> m ()
whenScenarioIs sid body = do
  mSid <- selectOne TheScenario
  when (mSid == Just sid) body

returnToThePitOfDespairId
  , returnToTheVanishingOfElinaHarperId
  , returnToInTooDeepId
  , returnToDevilReefId
  , returnToHorrorInHighGearId
  , returnToALightInTheFogId
  , returnToTheLairOfDagonId
  , returnToIntoTheMaelstromId
    :: ScenarioId
returnToThePitOfDespairId = ":return-to-the-innsmouth-conspiracy:018"
returnToTheVanishingOfElinaHarperId = ":return-to-the-innsmouth-conspiracy:022"
returnToInTooDeepId = ":return-to-the-innsmouth-conspiracy:028"
returnToDevilReefId = ":return-to-the-innsmouth-conspiracy:031"
returnToHorrorInHighGearId = ":return-to-the-innsmouth-conspiracy:035"
returnToALightInTheFogId = ":return-to-the-innsmouth-conspiracy:039"
returnToTheLairOfDagonId = ":return-to-the-innsmouth-conspiracy:043"
returnToIntoTheMaelstromId = ":return-to-the-innsmouth-conspiracy:048"

{- | Whether a card is any printing of one of these defs -- the box reprints several of the
campaign's cards, and a reprint is not equal to its original.
-}
isPrintingOfAny :: (HasCardCode a, HasCardDef a) => [CardDef] -> a -> Bool
isPrintingOfAny defs x = any ((`isPrintingOf` x) . toCardCode) defs

-- | How many of a face are in the bag right now.
inBag :: HasGame m => ChaosTokenFace -> m Int
inBag face = selectCount $ OnlyInBag (ChaosTokenFaceIs face)

-- | The Running side of each chase car; its ability 1 is the drive.
runningCars :: [CardDef]
runningCars = [Assets.thomasDawsonsCarRunning, Assets.elinaHarpersCarRunning]

-- | Both printings of the vessel: the box replaces the printed one with its own.
fishingVessels :: [CardDef]
fishingVessels = [Assets.fishingVessel, HBAssets.fishingVesselV2]

-- | The three Devil Reef relics, any one of which is a claimed key objective.
relics :: [CardDef]
relics = [Assets.wavewornIdol, Assets.awakenedMantle, Assets.headdressOfYhaNthlei]

-- | The box's three townspeople, all of whom "Friend of the People" wants in play at once.
townspeople :: [CardDef]
townspeople = [HBAssets.littleGemma, HBAssets.ronStalwick, HBAssets.roderick]

-- | Both gods, asleep or awake: "Go Back to Sleep" only cares that they are exhausted.
gods :: [CardDef]
gods =
  [ Enemies.dagonDeepInSlumber
  , Enemies.dagonDeepInSlumberIntoTheMaelstrom
  , Enemies.dagonAwakenedAndEnraged
  , Enemies.dagonAwakenedAndEnragedIntoTheMaelstrom
  , Enemies.hydraDeepInSlumber
  , Enemies.hydraAwakenedAndEnraged
  ]

{- | Earn "Friend of the People" when the townsperson just arriving completes the set. Only
the other two are looked up: this one is not in play yet.
-}
checkFriendOfThePeople :: (HasGame m, HasQueue Message m) => CardDef -> m ()
checkFriendOfThePeople def = when (def `elem` townspeople) do
  whenScenarioIs returnToTheVanishingOfElinaHarperId do
    let others = filter (/= def) townspeople
    whenM (allM (\d -> selectAny $ assetIs d <> AssetControlledBy Anyone) others) do
      earn FriendOfThePeople

-- | "Go Back to Sleep": both gods exhausted at once, during the investigator phase.
checkGodsAsleep :: (HasGame m, HasQueue Message m) => m ()
checkGodsAsleep = whenScenarioIs returnToIntoTheMaelstromId do
  phase <- gamePhase <$> getGame
  when (phase == InvestigationPhase) do
    dagon <- selectAny $ ExhaustedEnemy <> mapOneOf enemyIs (filter isDagon gods)
    hydra <- selectAny $ ExhaustedEnemy <> mapOneOf enemyIs (filter (not . isDagon) gods)
    when (dagon && hydra) $ earn GoBackToSleep
 where
  isDagon d =
    d
      `elem` [ Enemies.dagonDeepInSlumber
             , Enemies.dagonDeepInSlumberIntoTheMaelstrom
             , Enemies.dagonAwakenedAndEnraged
             , Enemies.dagonAwakenedAndEnragedIntoTheMaelstrom
             ]

eachLivingInvestigator :: HasGame m => (InvestigatorId -> m ()) -> m ()
eachLivingInvestigator f = select UneliminatedInvestigator >>= traverse_ f

-- Campaign store plumbing, mirroring the official campaign's.
bullDefeatsKey
  , deepOneDefeatedKey
  , vesselMovedKey
  , tenCursesKey
  , droveThisRoundKey
  , stepOnItBrokenKey
    :: Text
bullDefeatsKey = "rticAchBullDefeats"
deepOneDefeatedKey = "rticAchDeepOneDefeated"
vesselMovedKey = "rticAchVesselMoved"
tenCursesKey = "rticAchTenCurses"
droveThisRoundKey = "rticAchDroveThisRound"
stepOnItBrokenKey = "rticAchStepOnItBroken"

setStore :: (HasQueue Message m, ToJSON a) => Text -> a -> m ()
setStore k v = push $ Priority $ SetGlobal CampaignTarget (Key.fromText k) (toJSON v)

storedInt :: (HasCallStack, HasGame m) => Text -> m Int
storedInt k = fromMaybe 0 <$> stored k

storedFlag :: (HasCallStack, HasGame m) => Text -> m Bool
storedFlag k = fromMaybe False <$> stored k

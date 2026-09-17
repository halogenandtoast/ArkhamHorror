{- | The Unspeakable Oath's "Investigator Defeat" and Resolution 1.

Losing the whole table used to end the campaign outright. It should not: "if there
are not enough investigators to continue the campaign" is about the pool of
investigators the players can still pick from, and digitally that pool is never
empty. Resolution 1 therefore has to seat the replacements itself, because the two
bullets after it -- who takes the Clasp of Black Onyx, and who earns the
victory-display experience -- are about the investigators continuing the campaign.
-}
module Arkham.Scenario.Scenarios.TheUnspeakableOathSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign (lookupCampaign)
import Arkham.Campaign.Types (XpBreakdownStep (..))
import Arkham.Campaign.Types qualified as Campaign
import Arkham.CampaignLogKey (
  CampaignLogKey (DrivenInsaneInvestigators),
  recordedCardCodes,
  toCampaignLogKey,
 )
import Arkham.CampaignStep
import Arkham.Campaigns.ThePathToCarcosa.Key
import Arkham.Classes.HasGame (getGame)
import Arkham.Decklist.Type qualified as Decklist
import Arkham.Difficulty (Difficulty (Easy))
import Arkham.Enemy.CardDefs.ThePathToCarcosa.TheUnspeakableOath qualified as Enemies
import Arkham.Game.State
import Arkham.Helpers.Log (getHasRecord, getRecordSet)
import Arkham.Investigator.Types qualified as Investigator
import Arkham.Matcher qualified as Matcher
import Arkham.Projection (field)
import Arkham.Question
import Arkham.Scenario.Types qualified as Scenario
import Helpers.UltimatumsAndBoons (Ultimatum (..), withUltimatums)
import TestImport.New

-- | Seat the harness game inside a Path to Carcosa campaign at The Unspeakable Oath.
inTheUnspeakableOathWith :: (Scenario.ScenarioAttrs -> Scenario.ScenarioAttrs) -> TestAppT ()
inTheUnspeakableOathWith f = do
  let
    scenario' = overAttrs f (lookupScenario "03159" Easy)
    campaign' =
      overAttrs
        (\a -> a {Campaign.campaignStep = ScenarioStep (ScenarioId "03159")})
        (lookupCampaign "03" Easy)
  overTest \g -> g {gameMode = These campaign' scenario'}
  tick

inTheUnspeakableOath :: TestAppT ()
inTheUnspeakableOath = inTheUnspeakableOathWith id

-- | A decklist for the investigator a player picks up after theirs is lost.
rolandDecklist :: Decklist.ArkhamDBDecklist
rolandDecklist =
  Decklist.ArkhamDBDecklist
    { Decklist.slots = mempty
    , Decklist.sideSlots = mempty
    , Decklist.investigator_code = "01001"
    , Decklist.investigator_name = "Roland Banks"
    , Decklist.meta = Nothing
    , Decklist.taboo_id = Nothing
    , Decklist.url = Nothing
    , Decklist.decklist_id = Nothing
    , Decklist.decklist_name = Nothing
    }

isChooseUpgradeDeck :: Question Message -> Bool
isChooseUpgradeDeck q = case stripQuestionWrappers q of
  ChooseUpgradeDeck -> True
  _ -> False

{- | Defeat the only investigator, which is what drives the scenario into its
no-resolution ending, and read the resolution through. Leaves the game parked on
the replacement deck question.
-}
loseTheTable :: Investigator -> TestAppT ()
loseTheTable self = do
  pushAndRun $ InvestigatorDefeated (TestSource mempty) (toId self)
  chooseOnlyOption "read Investigator Defeat"
  chooseOnlyOption "read Resolution 1"

{- | Answer that deck question the way the deck endpoint does -- drop the seat's parked
question, then load the replacement decklist -- and hand back the new investigator.
-}
replaceWithRoland :: Investigator -> TestAppT InvestigatorId
replaceWithRoland self = do
  overTest \g -> g {gameQuestion = mempty}
  pushAndRun $ ReplaceInvestigator (toId self) rolandDecklist
  selectJust $ Matcher.InvestigatorWithTitle "Roland Banks"

-- | The campaign log's experience entry for this scenario.
scenarioXpBreakdown :: TestAppT (Maybe XpBreakdownStep)
scenarioXpBreakdown = do
  g <- getGame
  let steps = maybe [] (Campaign.campaignXpBreakdown . toAttrs) (modeCampaign (gameMode g))
  pure $ find ((== ScenarioStep (ScenarioId "03159")) . xbsStep) steps

spec :: Spec
spec = describe "The Unspeakable Oath" do
  describe "Resolution 1" do
    it "does not end the campaign when every investigator is driven insane" . gameTest $ \self -> do
      inTheUnspeakableOath
      loseTheTable self

      (gameGameState <$> getGame) `shouldSatisfyM` (/= IsOver)
      getHasRecord TheKingClaimedItsVictims `shouldReturn` True
      (recordedCardCodes <$> getRecordSet DrivenInsaneInvestigators)
        `shouldReturn` [toCardCode self]

    it "asks the player whose investigator was driven insane for a new one" . gameTest $ \self -> do
      inTheUnspeakableOath
      loseTheTable self

      pid <- getPlayer (toId self)
      (lookup pid . gameQuestion <$> getGame) `shouldSatisfyM` maybe False isChooseUpgradeDeck

    -- A replacement investigator builds their deck with no experience and then earns the
    -- victory display's, so it has to land on them -- not on the investigator they took
    -- over from, who is no longer in the campaign for the log to credit.
    it "awards the victory display experience to the replacement investigator" . gameTest $ \self -> do
      daniel <- genCard Enemies.danielChesterfield
      inTheUnspeakableOathWith \attrs -> attrs {Scenario.scenarioVictoryDisplay = [daniel]}
      loseTheTable self
      roland <- replaceWithRoland self

      field Investigator.InvestigatorXp roland `shouldReturn` 1

    it "credits the scenario's experience to the replacement in the campaign log" . gameTest $ \self -> do
      daniel <- genCard Enemies.danielChesterfield
      inTheUnspeakableOathWith \attrs -> attrs {Scenario.scenarioVictoryDisplay = [daniel]}
      loseTheTable self
      roland <- replaceWithRoland self

      (fmap xbsInvestigators <$> scenarioXpBreakdown) `shouldReturn` Just [roland]

    it "hands the clasp of black onyx to the replacement investigator" . gameTest $ \self -> do
      inTheUnspeakableOath
      run $ Record (toCampaignLogKey YouTookTheOnyxClasp)
      clasp <- genCard Assets.claspOfBlackOnyx
      run $ AddCampaignCardToDeck (toId self) DoNotShuffleIn clasp

      loseTheTable self
      roland <- replaceWithRoland self

      campaignId <- selectJust Matcher.TheCampaign
      storyCards <- field Campaign.CampaignStoryCards campaignId
      map toCardCode (findWithDefault [] roland storyCards)
        `shouldContain` [Assets.claspOfBlackOnyx.cardCode]
      map toCardCode (findWithDefault [] (toId self) storyCards) `shouldBe` []

    -- Ultimatum of Survival eliminates the player along with the investigator, so there
    -- really is nobody left to continue with.
    it "still ends the campaign under Ultimatum of Survival" . gameTest $ \self -> do
      inTheUnspeakableOath
      withUltimatums [UltimatumOfSurvival]
      pushAndRun $ InvestigatorDefeated (TestSource mempty) (toId self)
      chooseOnlyOption "read Investigator Defeat"

      (gameGameState <$> getGame) `shouldReturn` IsOver

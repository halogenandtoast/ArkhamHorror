{- | Return to the Dunwich Legacy achievement detection.

The detections live on the campaign entity, so most specs drive them with the
same messages the real scenarios emit (campaign log records, enemy defeats,
damage assignment) rather than replaying whole scenarios.
-}
module Arkham.Achievements.ReturnToTheDunwichLegacySpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey (CampaignLogKey (YouHaveIdentifiedTheSolution), recorded, toCampaignLogKey)
import Arkham.Campaigns.TheDunwichLegacy.Key
import Arkham.Difficulty
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher (AssetMatcher (AnyAsset))
import Arkham.Resolution
import Arkham.Source
import Arkham.Treachery.Cards qualified as Treacheries
import Helpers.Achievements
import Helpers.UltimatumsAndBoons (Ultimatum (..), withUltimatums)
import TestImport.New

winTheCampaign :: TestAppT ()
winTheCampaign = run $ Record (toCampaignLogKey TheInvestigatorsClosedTheTearInReality)

spec :: Spec
spec = describe "Return to the Dunwich Legacy achievements" $ do
  context "First Rule of Arkham" $ do
    it "is earned when the Necronomicon is destroyed" . gameTest $ \_ -> do
      asReturnToTheDunwichLegacy
      earned <- didEarnDunwich FirstRuleOfArkham
      run $ Record (toCampaignLogKey TheInvestigatorsDestroyedTheNecronomicon)
      earned `refShouldBe` True

    it "is not earned by taking custody" . gameTest $ \_ -> do
      asReturnToTheDunwichLegacy
      earned <- didEarnDunwich FirstRuleOfArkham
      run $ Record (toCampaignLogKey TheInvestigatorsTookCustodyOfTheNecronomicon)
      earned `refShouldBe` False

    it "is not earned in the base Dunwich Legacy campaign" . gameTest $ \_ -> do
      asAchievementCampaign "02" Easy
      earned <- didEarnDunwich FirstRuleOfArkham
      run $ Record (toCampaignLogKey TheInvestigatorsDestroyedTheNecronomicon)
      earned `refShouldBe` False

  context "No Void For You" $ do
    it "is earned when the museum is completed without defeating the Hunting Horror" . gameTest $ \_ -> do
      asReturnToTheDunwichLegacy
      earned <- didEarnDunwich NoVoidForYou
      run $ Record (toCampaignLogKey TheInvestigatorsTookCustodyOfTheNecronomicon)
      earned `refShouldBe` True

    it "is not earned once the Hunting Horror has been defeated" . gameTest $ \_ -> do
      asReturnToTheDunwichLegacy
      location <- testLocation
      horror <- testEnemyWithDef Enemies.huntingHorror id
      horror `spawnAt` location
      earned <- didEarnDunwich NoVoidForYou
      run $ Defeated (toTarget horror) (toCardId horror) (TestSource mempty) []
      -- the real Hunting Horror's forced leave-play ability prompts; resolve
      -- it so the queued defeat-tracker store write lands before the record
      chooseOnlyOption "hunting horror escapes to the void"
      run $ Record (toCampaignLogKey TheInvestigatorsDestroyedTheNecronomicon)
      earned `refShouldBe` False

  context "Eureka!" $ do
    it "is earned when the Strange Solution is identified" . gameTest $ \_ -> do
      asReturnToTheDunwichLegacy
      earned <- didEarnDunwich Eureka
      run $ Record YouHaveIdentifiedTheSolution
      earned `refShouldBe` True

  context "No Brood Left Behind" $ do
    it "is earned via the all-broods-defeated resolution" . gameTest $ \_ -> do
      asReturnToTheDunwichLegacy
      earned <- didEarnDunwich NoBroodLeftBehind
      run $ Record (toCampaignLogKey NoBroodEscapedIntoTheWild)
      earned `refShouldBe` True

    it "is earned when zero broods are recorded as escaped" . gameTest $ \_ -> do
      asReturnToTheDunwichLegacy
      earned <- didEarnDunwich NoBroodLeftBehind
      run $ RecordCount (toCampaignLogKey BroodEscapedIntoTheWild) 0
      earned `refShouldBe` True

    it "is not earned when broods escaped" . gameTest $ \_ -> do
      asReturnToTheDunwichLegacy
      earned <- didEarnDunwich NoBroodLeftBehind
      run $ RecordCount (toCampaignLogKey BroodEscapedIntoTheWild) 2
      earned `refShouldBe` False

  context "The Gang's All Here" $ do
    it "is earned when all five allies survive" . gameTest $ \_ -> do
      asReturnToTheDunwichLegacy
      earned <- didEarnDunwich TheGangsAllHere
      run $ Record (toCampaignLogKey DrHenryArmitageSurvivedTheDunwichLegacy)
      run $ Record (toCampaignLogKey ProfessorWarrenRiceSurvivedTheDunwichLegacy)
      run $ Record (toCampaignLogKey DrFrancisMorganSurvivedTheDunwichLegacy)
      run $ Record (toCampaignLogKey ZebulonWhateleySurvivedTheDunwichLegacy)
      earned `refShouldBe` False
      run $ Record (toCampaignLogKey EarlSawyerSurvivedTheDunwichLegacy)
      earned `refShouldBe` True

    it "is not earned with only four survivors" . gameTest $ \_ -> do
      asReturnToTheDunwichLegacy
      earned <- didEarnDunwich TheGangsAllHere
      run $ Record (toCampaignLogKey DrHenryArmitageSurvivedTheDunwichLegacy)
      run $ Record (toCampaignLogKey ProfessorWarrenRiceSurvivedTheDunwichLegacy)
      run $ Record (toCampaignLogKey DrFrancisMorganSurvivedTheDunwichLegacy)
      run $ Record (toCampaignLogKey ZebulonWhateleySurvivedTheDunwichLegacy)
      earned `refShouldBe` False

  context "winning the campaign" $ do
    it "earns Dunwich Expertise on Expert" . gameTest $ \_ -> do
      asReturnToTheDunwichLegacyWith Expert
      earned <- didEarnDunwich DunwichExpertise
      winTheCampaign
      earned `refShouldBe` True

    it "does not earn Dunwich Expertise below Expert" . gameTest $ \_ -> do
      asReturnToTheDunwichLegacyWith Hard
      earned <- didEarnDunwich DunwichExpertise
      winTheCampaign
      earned `refShouldBe` False

    it "earns Dunwich Line in the Sand with three active ultimatums" . gameTest $ \_ -> do
      asReturnToTheDunwichLegacy
      withUltimatums [UltimatumOfFailure, UltimatumOfHardship, UltimatumOfDread]
      earned <- didEarnDunwich DunwichLineInTheSand
      winTheCampaign
      earned `refShouldBe` True

    it "does not earn Dunwich Line in the Sand with only two" . gameTest $ \_ -> do
      asReturnToTheDunwichLegacy
      withUltimatums [UltimatumOfFailure, UltimatumOfHardship]
      earned <- didEarnDunwich DunwichLineInTheSand
      winTheCampaign
      earned `refShouldBe` False

    it "earns Tabula Rasa with a clean chaos bag" . gameTest $ \_ -> do
      asReturnToTheDunwichLegacy
      earned <- didEarnDunwich TabulaRasa
      winTheCampaign
      earned `refShouldBe` True

    it "does not earn Tabula Rasa with a Tablet in the bag" . gameTest $ \_ -> do
      asReturnToTheDunwichLegacy
      run $ SetChaosTokens [Tablet]
      earned <- didEarnDunwich TabulaRasa
      winTheCampaign
      earned `refShouldBe` False

  context "They Aren't Getting Away With This" $ do
    -- Pete is resigned first: everyone resigns through the tear before the
    -- winning resolution, which is exactly how a real win looks
    it "is earned winning as Ashcan Pete with Duke sacrificed" . gameTest $ \_ -> do
      asReturnToTheDunwichLegacy
      pete <- addInvestigator Investigators.ashcanPete
      updateInvestigator pete \a -> a {investigatorResigned = True}
      run $ RecordSetInsert (toCampaignLogKey SacrificedToYogSothoth) [recorded (toCardCode Assets.duke)]
      earned <- didEarnDunwich TheyArentGettingAwayWithThis
      winTheCampaign
      earned `refShouldBe` True

    it "is not earned without Ashcan Pete" . gameTest $ \_ -> do
      asReturnToTheDunwichLegacy
      run $ RecordSetInsert (toCampaignLogKey SacrificedToYogSothoth) [recorded (toCardCode Assets.duke)]
      earned <- didEarnDunwich TheyArentGettingAwayWithThis
      winTheCampaign
      earned `refShouldBe` False

    it "is not earned unless Duke was sacrificed" . gameTest $ \_ -> do
      asReturnToTheDunwichLegacy
      _pete <- addInvestigator Investigators.ashcanPete
      earned <- didEarnDunwich TheyArentGettingAwayWithThis
      winTheCampaign
      earned `refShouldBe` False

  context "What Is This Stuff, Anyway?" $ do
    it "is earned when the Alchemical Concoction defeats The Experiment" . gameTest $ \self -> do
      asReturnToTheDunwichLegacy
      location <- testLocation
      experiment <- testEnemyWithDef Enemies.theExperiment id
      experiment `spawnAt` location
      concoction <- self `putAssetIntoPlay` Assets.alchemicalConcoction
      earned <- didEarnDunwich WhatIsThisStuffAnyway
      run $ Defeated (toTarget experiment) (toCardId experiment) (AbilitySource (AssetSource concoction) 1) []
      earned `refShouldBe` True

    -- regression: the concoction removes itself from the game on the passed
    -- test, before the defeat lands — the detection must still resolve it
    it "is earned through the real fight" . gameTest $ \self -> do
      asReturnToTheDunwichLegacy
      withProp @"intellect" 10 self
      concoction <- self `putAssetIntoPlay` Assets.alchemicalConcoction
      experiment <- testEnemyWithDef Enemies.theExperiment id
      location <- testLocation
      setChaosTokens [Zero]
      run $ placedLocation location
      experiment `spawnAt` location
      self `moveTo` location
      -- solo health is 7 (printed) + 3 (per-player modifier); pre-damage so
      -- the concoction's 1+6 is the killing blow
      run $ PlaceDamage (TestSource mempty) (toTarget experiment) 3
      earned <- didEarnDunwich WhatIsThisStuffAnyway
      [doFight] <- concoction.abilities
      self `useAbility` doFight
      click "choose enemy"
      click "start skill test"
      click "apply results"
      earned `refShouldBe` True

    it "is not earned when The Experiment is defeated another way" . gameTest $ \self -> do
      asReturnToTheDunwichLegacy
      location <- testLocation
      experiment <- testEnemyWithDef Enemies.theExperiment id
      experiment `spawnAt` location
      _concoction <- self `putAssetIntoPlay` Assets.alchemicalConcoction
      earned <- didEarnDunwich WhatIsThisStuffAnyway
      run $ Defeated (toTarget experiment) (toCardId experiment) (TestSource mempty) []
      earned `refShouldBe` False

  context "Remind Me Not To Piss Her Off" $ do
    it "is earned defeating Seth Bishop while Naomi is controlled" . gameTest $ \self -> do
      asReturnToTheDunwichLegacy
      location <- testLocation
      seth <- testEnemyWithDef Enemies.sethBishop id
      seth `spawnAt` location
      _naomi <- self `putAssetIntoPlay` Assets.naomiOBannionRuthlessTactician
      earned <- didEarnDunwich RemindMeNotToPissHerOff
      run $ Defeated (toTarget seth) (toCardId seth) (TestSource mempty) []
      earned `refShouldBe` True

    it "works for the Thrall of Yog-Sothoth version" . gameTest $ \self -> do
      asReturnToTheDunwichLegacy
      location <- testLocation
      seth <- testEnemyWithDef Enemies.sethBishopThrallOfYogSothoth id
      seth `spawnAt` location
      _naomi <- self `putAssetIntoPlay` Assets.naomiOBannionRuthlessTactician
      earned <- didEarnDunwich RemindMeNotToPissHerOff
      run $ Defeated (toTarget seth) (toCardId seth) (TestSource mempty) []
      earned `refShouldBe` True

    it "is not earned without Naomi in play" . gameTest $ \_ -> do
      asReturnToTheDunwichLegacy
      location <- testLocation
      seth <- testEnemyWithDef Enemies.sethBishop id
      seth `spawnAt` location
      earned <- didEarnDunwich RemindMeNotToPissHerOff
      run $ Defeated (toTarget seth) (toCardId seth) (TestSource mempty) []
      earned `refShouldBe` False

  context "Bird Hunting" $ do
    it "is earned defeating three Whippoorwills in one turn" . gameTest $ \_ -> do
      asReturnToTheDunwichLegacy
      location <- testLocation
      earned <- didEarnDunwich BirdHunting
      let killWhippoorwill = do
            bird <- testEnemyWithDef Enemies.whippoorwill id
            bird `spawnAt` location
            run $ Defeated (toTarget bird) (toCardId bird) (TestSource mempty) []
      killWhippoorwill
      killWhippoorwill
      earned `refShouldBe` False
      killWhippoorwill
      earned `refShouldBe` True

    it "resets the count on a turn boundary" . gameTest $ \self -> do
      asReturnToTheDunwichLegacy
      location <- testLocation
      earned <- didEarnDunwich BirdHunting
      let killWhippoorwill = do
            bird <- testEnemyWithDef Enemies.whippoorwill id
            bird `spawnAt` location
            run $ Defeated (toTarget bird) (toCardId bird) (TestSource mempty) []
      killWhippoorwill
      killWhippoorwill
      run $ BeginTurn (toId self)
      killWhippoorwill
      earned `refShouldBe` False

  context "All Aboard" $ do
    -- the harness scenario is a shell, so the specs push Essex's DoStep
    -- resolution re-entry (the message the detection keys on) directly
    it "is earned when the Essex County Express completes with no passenger lost" . gameTest $ \_ -> do
      asReturnToTheDunwichLegacyScenario "51025"
      earned <- didEarnDunwich AllAboard
      run $ DoStep 1 $ ScenarioResolution (Resolution 1)
      earned `refShouldBe` True

    it "is not earned after a Helpless Passenger leaves play" . gameTest $ \self -> do
      asReturnToTheDunwichLegacyScenario "51025"
      passenger <- self `putAssetIntoPlay` Assets.helplessPassenger
      earned <- didEarnDunwich AllAboard
      run $ Discard Nothing GameSource (toTarget passenger)
      -- resolve the passenger's forced leave-play horror; the resolution is
      -- pushed to the END of the queue so the tracker store write (queued
      -- behind the discard cascade) lands first
      chooseOnlyOption "assign the passenger's horror"
      pushEnd $ DoStep 1 $ ScenarioResolution (Resolution 1)
      runMessages
      earned `refShouldBe` False

  context "Here We Go Again" $ do
    it "is earned when all three professors are in play" . gameTest $ \self -> do
      asReturnToTheDunwichLegacy
      earned <- didEarnDunwich HereWeGoAgain
      -- direct inserts for the first two (playing three allies would fight
      -- over the ally slot); the last one enters through the real play flow
      _ <- testAssetWithDef Assets.drHenryArmitage id self
      _ <- testAssetWithDef Assets.professorWarrenRice id self
      earned `refShouldBe` False
      _ <- self `putAssetIntoPlay` Assets.drFrancisMorgan
      earned `refShouldBe` True

  context "Beyond What Veil?" $ do
    it "is earned surviving all 10 damage" . gameTest $ \self -> do
      asReturnToTheDunwichLegacy
      withProp @"health" 20 self
      veil <- self `putTreacheryIntoPlay` Treacheries.beyondTheVeil
      earned <- didEarnDunwich BeyondWhatVeil
      run $ UseCardAbility (toId self) (TreacherySource veil) 1 [] NoPayment
      applyAllDamage
      earned `refShouldBe` True

    it "is earned even when some damage is soaked on an asset" . gameTest $ \self -> do
      asReturnToTheDunwichLegacy
      withProp @"health" 20 self
      _dog <- self `putAssetIntoPlay` Assets.guardDog
      veil <- self `putTreacheryIntoPlay` Treacheries.beyondTheVeil
      earned <- didEarnDunwich BeyondWhatVeil
      run $ UseCardAbility (toId self) (TreacherySource veil) 1 [] NoPayment
      chooseOptionMatching "soak one on the dog" \case
        AssetDamageLabel {} -> True
        _ -> False
      replicateM_ 9 $ chooseOptionMatching "take the rest" \case
        DamageLabel {} -> True
        _ -> False
      earned `refShouldBe` True

    it "is not earned when the hit is reduced below 10" . gameTest $ \self -> do
      asReturnToTheDunwichLegacy
      withProp @"health" 20 self
      veil <- self `putTreacheryIntoPlay` Treacheries.beyondTheVeil
      earned <- didEarnDunwich BeyondWhatVeil
      run
        $ InvestigatorDoAssignDamage
          (toId self)
          (AbilitySource (TreacherySource veil) 2)
          DamageAny
          AnyAsset
          0
          0
          (replicate 9 (toTarget self))
          []
      earned `refShouldBe` False

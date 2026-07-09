{- | Return to the Night of the Zealot achievement detection.

The detections live on the campaign entity, so most specs drive them with the
same messages the real scenarios emit (campaign log records, enemy defeats,
round boundaries) rather than replaying whole scenarios. Not covered here
because the harness can't reasonably drive them:

  * TheZealotsRevenge — needs a full attack skill test with Lita Chantler's
    reaction modifier active at the moment of defeat.
  * EvenDeathMayDie — Vault of Earthly Demise errors if it ever leaves play,
    and a synthetic Umordhoth defeat (without the real agenda ending the
    scenario first) runs the host-removal discard chain into that error.
-}
module Arkham.Achievements.ReturnToNightOfTheZealotSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey (recorded, toCampaignLogKey)
import Arkham.Campaigns.NightOfTheZealot.Key
import Arkham.Difficulty
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Settings (settingsAchievementsEnabled)
import Arkham.Location.Cards qualified as Locations
import Arkham.Resolution
import Arkham.Source
import Arkham.Trait (Trait (Cultist, Ghoul, Monster))
import Helpers.Achievements
import Helpers.UltimatumsAndBoons (Ultimatum (..), asCampaign, withUltimatums)
import TestImport.New hiding (Cultist)

winTheCampaign :: TestAppT ()
winTheCampaign = run $ Record (toCampaignLogKey TheRitualToSummonUmordhothWasBroken)

spec :: Spec
spec = describe "Return to the Night of the Zealot achievements" $ do
  context "I Don't Trust Her" $ do
    it "is earned by declining Lita Chantler at the resolution" . gameTest $ \_ -> do
      asReturnToNightOfTheZealotScenario "50011"
      earned <- didEarn IDontTrustHer
      run $ ScenarioResolution (Resolution 1)
      chooseOnlyOption "continue resolution story"
      chooseOptionMatching "decline Lita" \case
        Label {} -> True
        _ -> False
      earned `refShouldBe` True

    it "is not earned by adding Lita to a deck" . gameTest $ \_ -> do
      asReturnToNightOfTheZealotScenario "50011"
      earned <- didEarn IDontTrustHer
      run $ ScenarioResolution (Resolution 1)
      chooseOnlyOption "continue resolution story"
      chooseOptionMatching "add Lita" \case
        PortraitLabel {} -> True
        _ -> False
      earned `refShouldBe` False

  context "Insurance Doesn't Cover Ghouls" $ do
    it "is earned when your house burns to the ground" . gameTest $ \_ -> do
      asReturnToNightOfTheZealot
      earned <- didEarn InsuranceDoesntCoverGhouls
      run $ Record (toCampaignLogKey YourHouseHasBurnedToTheGround)
      earned `refShouldBe` True

    it "is not earned while achievements are disabled" . gameTest $ \_ -> do
      asReturnToNightOfTheZealot
      overTest \g -> g {gameSettings = (gameSettings g) {settingsAchievementsEnabled = False}}
      earned <- didEarn InsuranceDoesntCoverGhouls
      run $ Record (toCampaignLogKey YourHouseHasBurnedToTheGround)
      earned `refShouldBe` False

    it "is not earned in the base Night of the Zealot campaign" . gameTest $ \_ -> do
      asCampaign -- campaign "01"
      earned <- didEarn InsuranceDoesntCoverGhouls
      run $ Record (toCampaignLogKey YourHouseHasBurnedToTheGround)
      earned `refShouldBe` False

  context "Conspiracy of Silence" $ do
    it "is earned when six unique cultists are recorded as interrogated" . gameTest $ \_ -> do
      asReturnToNightOfTheZealot
      earned <- didEarn ConspiracyOfSilence
      run
        $ RecordSetInsert (toCampaignLogKey CultistsWeInterrogated)
        $ map
          (recorded . toCardCode)
          [ Enemies.wolfManDrew
          , Enemies.hermanCollins
          , Enemies.peterWarren
          , Enemies.victoriaDevereux
          , Enemies.ruthTurner
          , Enemies.theMaskedHunter
          ]
      earned `refShouldBe` True

    it "is not earned with only five" . gameTest $ \_ -> do
      asReturnToNightOfTheZealot
      earned <- didEarn ConspiracyOfSilence
      run
        $ RecordSetInsert (toCampaignLogKey CultistsWeInterrogated)
        $ map
          (recorded . toCardCode)
          [ Enemies.wolfManDrew
          , Enemies.hermanCollins
          , Enemies.peterWarren
          , Enemies.victoriaDevereux
          , Enemies.ruthTurner
          ]
      earned `refShouldBe` False

  context "winning the campaign" $ do
    it "earns Arkham Expertise on Expert" . gameTest $ \_ -> do
      asReturnToNightOfTheZealotWith Expert
      earned <- didEarn ArkhamExpertise
      winTheCampaign
      earned `refShouldBe` True

    it "does not earn Arkham Expertise below Expert" . gameTest $ \_ -> do
      asReturnToNightOfTheZealotWith Hard
      earned <- didEarn ArkhamExpertise
      winTheCampaign
      earned `refShouldBe` False

    it "earns Zealot Line in the Sand with three active ultimatums" . gameTest $ \_ -> do
      asReturnToNightOfTheZealot
      withUltimatums [UltimatumOfFailure, UltimatumOfHardship, UltimatumOfDread]
      earned <- didEarn ZealotLineInTheSand
      winTheCampaign
      earned `refShouldBe` True

    it "does not earn Zealot Line in the Sand with only two" . gameTest $ \_ -> do
      asReturnToNightOfTheZealot
      withUltimatums [UltimatumOfFailure, UltimatumOfHardship]
      earned <- didEarn ZealotLineInTheSand
      winTheCampaign
      earned `refShouldBe` False

  context "They're Just Misunderstood / Umordhoth's Favor" $ do
    it "earns both when nothing relevant was defeated" . gameTest $ \_ -> do
      asReturnToNightOfTheZealot
      misunderstood <- didEarn TheyreJustMisunderstood
      favor <- didEarn UmordhothsFavor
      winTheCampaign
      misunderstood `refShouldBe` True
      favor `refShouldBe` True

    it "a Ghoul defeat forfeits Umordhoth's Favor but not They're Just Misunderstood" . gameTest $ \_ -> do
      asReturnToNightOfTheZealot
      location <- testLocation
      enemy <- testEnemy
      enemy `spawnAt` location
      misunderstood <- didEarn TheyreJustMisunderstood
      favor <- didEarn UmordhothsFavor
      run $ Defeated (toTarget enemy) (toCardId enemy) (TestSource mempty) [Ghoul]
      winTheCampaign
      favor `refShouldBe` False
      misunderstood `refShouldBe` True

    it "a unique Cultist defeat forfeits They're Just Misunderstood" . gameTest $ \_ -> do
      asReturnToNightOfTheZealot
      location <- testLocation
      drew <- testEnemyWithDef Enemies.wolfManDrew id
      drew `spawnAt` location
      misunderstood <- didEarn TheyreJustMisunderstood
      run $ Defeated (toTarget drew) (toCardId drew) (TestSource mempty) [Cultist]
      winTheCampaign
      misunderstood `refShouldBe` False

    it "a non-unique Cultist defeat does not forfeit They're Just Misunderstood" . gameTest $ \_ -> do
      asReturnToNightOfTheZealot
      location <- testLocation
      enemy <- testEnemy
      enemy `spawnAt` location
      misunderstood <- didEarn TheyreJustMisunderstood
      run $ Defeated (toTarget enemy) (toCardId enemy) (TestSource mempty) [Cultist]
      winTheCampaign
      misunderstood `refShouldBe` True

  context "Break the Circle" $ do
    it "is earned when the ritual is broken with Umordhoth never spawned" . gameTest $ \_ -> do
      asReturnToNightOfTheZealot
      earned <- didEarn BreakTheCircle
      winTheCampaign
      earned `refShouldBe` True

    it "is not earned if Umordhoth is in play at the resolution" . gameTest $ \_ -> do
      asReturnToNightOfTheZealot
      location <- testLocation
      umordhoth <- testEnemyWithDef Enemies.umordhoth id
      umordhoth `spawnAt` location
      earned <- didEarn BreakTheCircle
      winTheCampaign
      earned `refShouldBe` False

    it "is not earned via the repelled-Umordhoth resolution" . gameTest $ \_ -> do
      asReturnToNightOfTheZealot
      earned <- didEarn BreakTheCircle
      run $ Record (toCampaignLogKey TheInvestigatorsRepelledUmordoth)
      earned `refShouldBe` False

  context "Do You Get It Now?" $ do
    it "is earned when Ghoul Priest is defeated at Billy Cooper's location" . gameTest $ \_ -> do
      asReturnToNightOfTheZealot
      easttown <- testLocationWithDef Locations.easttown id
      priest <- testEnemyWithDef Enemies.ghoulPriest id
      billy <- testEnemyWithDef Enemies.billyCooper id
      priest `spawnAt` easttown
      billy `spawnAt` easttown
      earned <- didEarn DoYouGetItNow
      run $ Defeated (toTarget priest) (toCardId priest) (TestSource mempty) [Ghoul, Monster]
      -- the real Billy Cooper's Forced ability prompts on this defeat; the
      -- queued earn only processes once the prompt resolves
      chooseOnlyOption "trigger Billy Cooper"
      earned `refShouldBe` True

    it "is not earned when Billy Cooper is elsewhere" . gameTest $ \_ -> do
      asReturnToNightOfTheZealot
      easttown <- testLocationWithDef Locations.easttown id
      other <- testLocation
      priest <- testEnemyWithDef Enemies.ghoulPriest id
      billy <- testEnemyWithDef Enemies.billyCooper id
      priest `spawnAt` other
      billy `spawnAt` easttown
      earned <- didEarn DoYouGetItNow
      run $ Defeated (toTarget priest) (toCardId priest) (TestSource mempty) [Ghoul, Monster]
      earned `refShouldBe` False

  context "Pinch Hitter" $ do
    it "is earned when one Baseball Bat defeats three Ghouls" . gameTest $ \self -> do
      asReturnToNightOfTheZealot
      bat <- testAssetWithDef Assets.baseballBat id self
      location <- testLocation
      earned <- didEarn PinchHitter
      let killWithBat = do
            enemy <- testEnemy
            enemy `spawnAt` location
            run $ Defeated (toTarget enemy) (toCardId enemy) (AbilitySource (toSource bat) 1) [Ghoul]
      killWithBat
      killWithBat
      earned `refShouldBe` False
      killWithBat
      earned `refShouldBe` True

    it "does not pool kills across different bats" . gameTest $ \self -> do
      asReturnToNightOfTheZealot
      bat1 <- testAssetWithDef Assets.baseballBat id self
      bat2 <- testAssetWithDef Assets.baseballBat id self
      location <- testLocation
      earned <- didEarn PinchHitter
      let killWith bat = do
            enemy <- testEnemy
            enemy `spawnAt` location
            run $ Defeated (toTarget enemy) (toCardId enemy) (AbilitySource (toSource bat) 1) [Ghoul]
      killWith bat1
      killWith bat1
      killWith bat2
      earned `refShouldBe` False

  context "But Do I Have To?" $ do
    it "is earned after everyone stays in Your House for three rounds" . gameTest $ \self -> do
      asReturnToNightOfTheZealotScenario "50025"
      house <- testLocationWithDef Locations.yourHouse id
      self `moveTo` house
      earned <- didEarn ButDoIHaveTo
      run EndRound
      run EndRound
      earned `refShouldBe` False
      run EndRound
      earned `refShouldBe` True

    it "is not earned if an investigator leaves during the first three rounds" . gameTest $ \self -> do
      asReturnToNightOfTheZealotScenario "50025"
      house <- testLocationWithDef Locations.yourHouse id
      rivertown <- testLocationWithDef Locations.rivertown id
      self `moveTo` house
      earned <- didEarn ButDoIHaveTo
      run EndRound
      self `moveTo` rivertown
      self `moveTo` house
      run EndRound
      run EndRound
      earned `refShouldBe` False

    it "is not earned when Your House is not in play" . gameTest $ \self -> do
      asReturnToNightOfTheZealotScenario "50025"
      rivertown <- testLocationWithDef Locations.rivertown id
      self `moveTo` rivertown
      earned <- didEarn ButDoIHaveTo
      run EndRound
      run EndRound
      run EndRound
      earned `refShouldBe` False

  context "Tour of Arkham" $ do
    it "is earned once every once-per-game location in play has triggered" . gameTest $ \self -> do
      asReturnToNightOfTheZealotScenario "50025"
      northside <- testLocationWithDef Locations.northside id
      stMarys <- testLocationWithDef Locations.stMarysHospital id
      -- no once-per-game ability, so it imposes no requirement
      _graveyard <- testLocationWithDef Locations.graveyard id
      self `moveTo` northside
      earned <- didEarn TourOfArkham
      run $ UseCardAbility (toId self) (toSource northside) 1 [] NoPayment
      earned `refShouldBe` False
      run $ UseCardAbility (toId self) (toSource stMarys) 1 [] NoPayment
      earned `refShouldBe` True

    it "is not earned outside The Midnight Masks" . gameTest $ \self -> do
      asReturnToNightOfTheZealot -- harness scenario is The Gathering
      northside <- testLocationWithDef Locations.northside id
      self `moveTo` northside
      earned <- didEarn TourOfArkham
      run $ UseCardAbility (toId self) (toSource northside) 1 [] NoPayment
      earned `refShouldBe` False

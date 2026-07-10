{- | Return to The Path to Carcosa achievement detection.

The detections live on the campaign entity, so most specs drive them with the
same messages the real scenarios emit (campaign log records, enemy defeats,
ability uses) rather than replaying whole scenarios.

"Say My Name" has no spec: it is an above-the-table verbal feat (speak the
name of Hastur aloud seven times) with no engine signal, so there is nothing
for the harness to drive.
-}
module Arkham.Achievements.ReturnToThePathToCarcosaSpec (spec) where

import Arkham.Agenda.Sequence (AgendaSequence (..), AgendaSide (..))
import Arkham.Agenda.Types (AgendaAttrs (..))
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey (recorded, toCampaignLogKey)
import Arkham.Campaigns.ThePathToCarcosa.Key
import Arkham.Difficulty
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Base (LocationAttrs (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Placement (Placement (HiddenInHand))
import Arkham.Source
import Arkham.Target
import Arkham.Treachery.Cards qualified as Treacheries
import Helpers.Achievements
import Helpers.UltimatumsAndBoons (Ultimatum (..), withUltimatums)
import TestImport.New

winTheCampaign :: TestAppT ()
winTheCampaign =
  run $ Record (toCampaignLogKey TheInvestigatorsPreventedHasturFromEscapingHisPrison)

spec :: Spec
spec = describe "Return to The Path to Carcosa achievements" $ do
  context "Fair Warning" $ do
    it "is earned after three Royal Emissary defeats in Curtain Call" . gameTest $ \_ -> do
      asReturnToThePathToCarcosaScenario "52014"
      location <- testLocation
      earned <- didEarnCarcosa FairWarning
      let killEmissary = do
            emissary <- testEnemyWithDef Enemies.royalEmissary id
            emissary `spawnAt` location
            run $ Defeated (toTarget emissary) (toCardId emissary) (TestSource mempty) []
      killEmissary
      killEmissary
      earned `refShouldBe` False
      killEmissary
      earned `refShouldBe` True

    it "is not earned outside Curtain Call" . gameTest $ \_ -> do
      asReturnToThePathToCarcosa
      location <- testLocation
      earned <- didEarnCarcosa FairWarning
      let killEmissary = do
            emissary <- testEnemyWithDef Enemies.royalEmissary id
            emissary `spawnAt` location
            run $ Defeated (toTarget emissary) (toCardId emissary) (TestSource mempty) []
      killEmissary
      killEmissary
      killEmissary
      earned `refShouldBe` False

  context "First Steps" $ do
    it "is earned when all five VIPs are interviewed" . gameTest $ \_ -> do
      asReturnToThePathToCarcosa
      earned <- didEarnCarcosa FirstSteps
      run
        $ RecordSetInsert
          (toCampaignLogKey VIPsInterviewed)
          [ recorded (toCardCode Assets.constanceDumaine)
          , recorded (toCardCode Assets.jordanPerry)
          , recorded (toCardCode Assets.ishimaruHaruko)
          , recorded (toCardCode Assets.sebastienMoreau)
          , recorded (toCardCode Assets.ashleighClarke)
          ]
      earned `refShouldBe` True

    it "is not earned with only four VIPs interviewed" . gameTest $ \_ -> do
      asReturnToThePathToCarcosa
      earned <- didEarnCarcosa FirstSteps
      run
        $ RecordSetInsert
          (toCampaignLogKey VIPsInterviewed)
          [ recorded (toCardCode Assets.constanceDumaine)
          , recorded (toCardCode Assets.jordanPerry)
          , recorded (toCardCode Assets.ishimaruHaruko)
          , recorded (toCardCode Assets.sebastienMoreau)
          ]
      earned `refShouldBe` False

  context "Crashing the Party" $ do
    it "is earned defeating the Lunatic Dianne Devine" . gameTest $ \_ -> do
      asReturnToThePathToCarcosa
      location <- testLocation
      dianne <- testEnemyWithDef Enemies.dianneDevineKnowsWhatYoureUpTo id
      dianne `spawnAt` location
      earned <- didEarnCarcosa CrashingTheParty
      run $ Defeated (toTarget dianne) (toCardId dianne) (TestSource mempty) []
      earned `refShouldBe` True

    it "is not earned defeating a different enemy" . gameTest $ \_ -> do
      asReturnToThePathToCarcosa
      location <- testLocation
      other <- testEnemyWithDef Enemies.royalEmissary id
      other `spawnAt` location
      earned <- didEarnCarcosa CrashingTheParty
      run $ Defeated (toTarget other) (toCardId other) (TestSource mempty) []
      earned `refShouldBe` False

  context "For Prying Eyes" $ do
    it "is earned resolving Echoes of the Past with the Hidden Library cleared" . gameTest $ \_ -> do
      asReturnToThePathToCarcosaScenario "52028"
      _ <-
        testLocationWithDef Locations.hiddenLibrary
          $ \a -> a {locationRevealed = True, locationWithoutClues = True}
      earned <- didEarnCarcosa ForPryingEyes
      run $ Record (toCampaignLogKey YouTookTheOnyxClasp)
      earned `refShouldBe` True

    it "is not earned if the Hidden Library still has clues" . gameTest $ \_ -> do
      asReturnToThePathToCarcosaScenario "52028"
      _ <- testLocationWithDef Locations.hiddenLibrary $ \a -> a {locationRevealed = True}
      earned <- didEarnCarcosa ForPryingEyes
      run $ Record (toCampaignLogKey YouTookTheOnyxClasp)
      earned `refShouldBe` False

  context "The Cuckoo's Nest" $ do
    it "is earned resigning with Daniel Chesterfield" . gameTest $ \self -> do
      asReturnToThePathToCarcosa
      daniel <- self `putAssetIntoPlay` Assets.danielChesterfield
      earned <- didEarnCarcosa TheCuckoosNest
      run $ ResignWith (toTarget daniel)
      earned `refShouldBe` True

    it "is not earned resigning with a different asset" . gameTest $ \self -> do
      asReturnToThePathToCarcosa
      other <- self `putAssetIntoPlay` Assets.guardDog
      earned <- didEarnCarcosa TheCuckoosNest
      run $ ResignWith (toTarget other)
      earned `refShouldBe` False

  context "Take a Look at This!" $ do
    it "is earned parleying with the Host of Insanity" . gameTest $ \self -> do
      asReturnToThePathToCarcosa
      location <- testLocation
      host <- testEnemyWithDef Enemies.hostOfInsanity id
      host `spawnAt` location
      earned <- didEarnCarcosa TakeALookAtThis
      run $ UseCardAbility (toId self) (EnemySource (toId host)) 1 [] NoPayment
      earned `refShouldBe` True

    it "is not earned using another enemy's ability" . gameTest $ \self -> do
      asReturnToThePathToCarcosa
      location <- testLocation
      other <- testEnemyWithDef Enemies.royalEmissary id
      other `spawnAt` location
      earned <- didEarnCarcosa TakeALookAtThis
      run $ UseCardAbility (toId self) (EnemySource (toId other)) 1 [] NoPayment
      earned `refShouldBe` False

  context "The Path of Death" $ do
    it "is earned using the Secret Passage shortcut" . gameTest $ \self -> do
      asReturnToThePathToCarcosa
      passage <- testLocationWithDef Locations.returnToSecretPassage id
      earned <- didEarnCarcosa ThePathOfDeath
      run $ UseCardAbility (toId self) (LocationSource (toId passage)) 1 [] NoPayment
      earned `refShouldBe` True

    it "is not earned using another location's ability" . gameTest $ \self -> do
      asReturnToThePathToCarcosa
      location <- testLocation
      earned <- didEarnCarcosa ThePathOfDeath
      run $ UseCardAbility (toId self) (LocationSource (toId location)) 1 [] NoPayment
      earned `refShouldBe` False

  context "Guessing Game" $ do
    it "is earned racing one deck's final agenda ahead of the other's first" . gameTest $ \_ -> do
      asReturnToThePathToCarcosaScenario "52054"
      deck1 <- testAgenda "03276a" \a -> a {agendaDeckId = 1, agendaSequence = Sequence 2 A}
      _deck2 <- testAgenda "03278" \a -> a {agendaDeckId = 2, agendaSequence = Sequence 1 C}
      earned <- didEarnCarcosa GuessingGame
      run $ AdvanceAgendaBy (toId deck1) AgendaAdvancedWithOther
      earned `refShouldBe` True

    it "is not earned when the other deck's first agenda already advanced" . gameTest $ \_ -> do
      asReturnToThePathToCarcosaScenario "52054"
      deck1 <- testAgenda "03276a" \a -> a {agendaDeckId = 1, agendaSequence = Sequence 2 A}
      _deck2 <- testAgenda "03279a" \a -> a {agendaDeckId = 2, agendaSequence = Sequence 2 C}
      earned <- didEarnCarcosa GuessingGame
      run $ AdvanceAgendaBy (toId deck1) AgendaAdvancedWithOther
      earned `refShouldBe` False

  context "Hastur Made Me Do It" $ do
    it "is earned defeating Hastur with a Possession treachery in hand" . gameTest $ \self -> do
      asReturnToThePathToCarcosa
      location <- testLocation
      possession <- self `putTreacheryIntoPlay` Treacheries.possessionMurderous
      run $ PlaceTreachery possession (HiddenInHand (toId self))
      hastur <- testEnemyWithDef Enemies.hasturTheTatteredKing id
      hastur `spawnAt` location
      earned <- didEarnCarcosa HasturMadeMeDoIt
      run $ Defeated (toTarget hastur) (toCardId hastur) (TestSource mempty) []
      earned `refShouldBe` True

    it "is not earned without a Possession treachery in hand" . gameTest $ \_ -> do
      asReturnToThePathToCarcosa
      location <- testLocation
      hastur <- testEnemyWithDef Enemies.hasturTheTatteredKing id
      hastur `spawnAt` location
      earned <- didEarnCarcosa HasturMadeMeDoIt
      run $ Defeated (toTarget hastur) (toCardId hastur) (TestSource mempty) []
      earned `refShouldBe` False

  context "winning the campaign" $ do
    it "earns Carcosa Expertise on Expert" . gameTest $ \_ -> do
      asReturnToThePathToCarcosaWith Expert
      earned <- didEarnCarcosa CarcosaExpertise
      winTheCampaign
      earned `refShouldBe` True

    it "does not earn Carcosa Expertise below Expert" . gameTest $ \_ -> do
      asReturnToThePathToCarcosaWith Hard
      earned <- didEarnCarcosa CarcosaExpertise
      winTheCampaign
      earned `refShouldBe` False

    it "earns Line in the Sand with three active ultimatums" . gameTest $ \_ -> do
      asReturnToThePathToCarcosa
      withUltimatums [UltimatumOfFailure, UltimatumOfHardship, UltimatumOfDread]
      earned <- didEarnCarcosa CarcosaLineInTheSand
      winTheCampaign
      earned `refShouldBe` True

    it "does not earn Line in the Sand with only two" . gameTest $ \_ -> do
      asReturnToThePathToCarcosa
      withUltimatums [UltimatumOfFailure, UltimatumOfHardship]
      earned <- didEarnCarcosa CarcosaLineInTheSand
      winTheCampaign
      earned `refShouldBe` False

    it "earns The Path is False with 8 Doubt" . gameTest $ \_ -> do
      asReturnToThePathToCarcosa
      recordCount Doubt 8
      earned <- didEarnCarcosa ThePathIsFalse
      winTheCampaign
      earned `refShouldBe` True

    it "does not earn The Path is False without 8 Doubt" . gameTest $ \_ -> do
      asReturnToThePathToCarcosa
      recordCount Doubt 7
      earned <- didEarnCarcosa ThePathIsFalse
      winTheCampaign
      earned `refShouldBe` False

    it "earns The Path is Real with 8 Conviction" . gameTest $ \_ -> do
      asReturnToThePathToCarcosa
      recordCount Conviction 8
      earned <- didEarnCarcosa ThePathIsReal
      winTheCampaign
      earned `refShouldBe` True

    it "earns The Path is Mine with less than 2 total" . gameTest $ \_ -> do
      asReturnToThePathToCarcosa
      recordCount Doubt 1
      recordCount Conviction 0
      earned <- didEarnCarcosa ThePathIsMine
      winTheCampaign
      earned `refShouldBe` True

    it "does not earn The Path is Mine with 2 or more total" . gameTest $ \_ -> do
      asReturnToThePathToCarcosa
      recordCount Doubt 1
      recordCount Conviction 1
      earned <- didEarnCarcosa ThePathIsMine
      winTheCampaign
      earned `refShouldBe` False

  context "Get Back Here" $ do
    it "is earned winning with The Man never in play at a scenario end" . gameTest $ \_ -> do
      asReturnToThePathToCarcosa
      earned <- didEarnCarcosa GetBackHere
      winTheCampaign
      earned `refShouldBe` True

    it "is not earned if The Man is in play at the winning scenario's end" . gameTest $ \_ -> do
      asReturnToThePathToCarcosa
      location <- testLocation
      man <- testEnemyWithDef Enemies.theManInThePallidMask id
      man `spawnAt` location
      earned <- didEarnCarcosa GetBackHere
      winTheCampaign
      earned `refShouldBe` False

    it "is not earned if a prior scenario resolved with The Man in play" . gameTest $ \_ -> do
      asReturnToThePathToCarcosaScenario "52048"
      location <- testLocation
      man <- testEnemyWithDef Enemies.theManInThePallidMask id
      man `spawnAt` location
      earned <- didEarnCarcosa GetBackHere
      -- a scenario resolves (a plain narrative Record) while The Man is on the
      -- board, disqualifying the run for good
      run $ Record (toCampaignLogKey TheStrangerIsOnToYou)
      run $ Defeated (toTarget man) (toCardId man) (TestSource mempty) []
      winTheCampaign
      earned `refShouldBe` False

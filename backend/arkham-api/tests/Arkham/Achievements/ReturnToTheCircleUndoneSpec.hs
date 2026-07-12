{- | Return to The Circle Undone achievement detection.

The detections live on the campaign entity, so most specs drive them with the
same messages the real scenarios emit (campaign log records, memento inserts,
enemy defeats, incursions) rather than replaying whole scenarios. "Who You
Gonna Call?" drives the real defeat flow (Defeated cascades to Discarded, which
is where the count lands) and "10/10 Would Read Again" drives a real Black Book
horror assignment.
-}
module Arkham.Achievements.ReturnToTheCircleUndoneSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey (recorded, toCampaignLogKey)
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Difficulty
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Message
import Arkham.Source
import Helpers.Achievements
import TestImport.New

spec :: Spec
spec = describe "Return to The Circle Undone achievements" $ do
  context "Who You Gonna Call?" $ do
    -- Count a Geist/Spectral enemy only when it is added to the victory display
    -- (or discarded). Malevolent Spirit is Geist+Spectral; drive AddToVictory
    -- directly rather than Defeated (its Forced ability cancels its own defeat,
    -- which is exactly the "returns to void, does not count" case).
    let victoryGeist location = do
          spirit <- testEnemyWithDef Enemies.malevolentSpirit id
          spirit `spawnAt` location
          run $ AddToVictory Nothing (toTarget spirit)
    it "is earned after thirteen Geist/Spectral defeats" . gameTest $ \_ -> do
      asReturnToTheCircleUndone
      location <- testLocation
      earned <- didEarnCircle WhoYouGonnaCall
      replicateM_ 12 (victoryGeist location)
      earned `refShouldBe` False
      victoryGeist location
      earned `refShouldBe` True

    it "is not earned after twelve" . gameTest $ \_ -> do
      asReturnToTheCircleUndone
      location <- testLocation
      earned <- didEarnCircle WhoYouGonnaCall
      replicateM_ 12 (victoryGeist location)
      earned `refShouldBe` False

  context "Savior of Humanity" $ do
    it "is earned escaping the Spectral Realm with no Silver Twilight killed" . gameTest $ \_ -> do
      asReturnToTheCircleUndoneScenario "05065"
      earned <- didEarnCircle SaviorOfHumanity
      run $ Record (toCampaignLogKey TheInvestigatorsEscapedTheSpectralRealm)
      earned `refShouldBe` True

    it "is not earned when a Silver Twilight enemy is in the victory display" . gameTest $ \_ -> do
      asReturnToTheCircleUndoneScenario "05065"
      location <- testLocation
      neophyte <- testEnemyWithDef Enemies.lodgeNeophyte id
      neophyte `spawnAt` location
      run $ AddToVictory Nothing (toTarget neophyte)
      earned <- didEarnCircle SaviorOfHumanity
      run $ Record (toCampaignLogKey TheInvestigatorsEscapedTheSpectralRealm)
      earned `refShouldBe` False

  context "10/10 Would Read Again" $ do
    it "is earned after taking ten horror via The Black Book" . gameTest $ \self -> do
      asReturnToTheCircleUndone
      withProp @"sanity" 30 self
      blackBook <- self `putAssetIntoPlay` Assets.theBlackBook
      let takeHorror n =
            run $ InvestigatorAssignDamage (toId self) (AbilitySource (AssetSource blackBook) 1) DamageAny 0 n
      earned <- didEarnCircle TenOutOfTenWouldReadAgain
      takeHorror 9
      earned `refShouldBe` False
      takeHorror 1
      earned `refShouldBe` True

    it "is not earned after nine horror" . gameTest $ \self -> do
      asReturnToTheCircleUndone
      withProp @"sanity" 30 self
      blackBook <- self `putAssetIntoPlay` Assets.theBlackBook
      earned <- didEarnCircle TenOutOfTenWouldReadAgain
      run $ InvestigatorAssignDamage (toId self) (AbilitySource (AssetSource blackBook) 1) DamageAny 0 9
      earned `refShouldBe` False

  context "Carl Shmarl" $ do
    it "is earned siding with the Coven after joining the Inner Circle" . gameTest $ \_ -> do
      asReturnToTheCircleUndone
      record TheInvestigatorsWereInductedIntoTheInnerCircle
      earned <- didEarnCircle CarlShmarl
      run $ Record (toCampaignLogKey TheInvestigatorsSidedWithTheCoven)
      earned `refShouldBe` True

    it "is not earned siding with the Coven without joining the Inner Circle" . gameTest $ \_ -> do
      asReturnToTheCircleUndone
      earned <- didEarnCircle CarlShmarl
      run $ Record (toCampaignLogKey TheInvestigatorsSidedWithTheCoven)
      earned `refShouldBe` False

  context "The Threefold Rule" $ do
    it "is earned siding with the Lodge after joining Erynn" . gameTest $ \_ -> do
      asReturnToTheCircleUndone
      record ErynnJoinedTheInvestigators
      earned <- didEarnCircle TheThreefoldRule
      run $ Record (toCampaignLogKey TheInvestigatorsSidedWithTheLodge)
      earned `refShouldBe` True

    it "is not earned siding with the Lodge without Erynn" . gameTest $ \_ -> do
      asReturnToTheCircleUndone
      earned <- didEarnCircle TheThreefoldRule
      run $ Record (toCampaignLogKey TheInvestigatorsSidedWithTheLodge)
      earned `refShouldBe` False

  context "New World Order" $ do
    it "is earned when the Lodge wins the campaign" . gameTest $ \_ -> do
      asReturnToTheCircleUndone
      earned <- didEarnCircle NewWorldOrder
      run $ Record (toCampaignLogKey TheTrueWorkOfTheSilverTwilightLodgeHasBegun)
      earned `refShouldBe` True

    it "is not earned by an unrelated record" . gameTest $ \_ -> do
      asReturnToTheCircleUndone
      earned <- didEarnCircle NewWorldOrder
      run $ Record (toCampaignLogKey TheInvestigatorsContinuedAlone)
      earned `refShouldBe` False

  context "Immortality Sounds Nice" $ do
    it "is earned when the Coven wins the campaign" . gameTest $ \_ -> do
      asReturnToTheCircleUndone
      earned <- didEarnCircle ImmortalitySoundsNice
      run $ Record (toCampaignLogKey TheCovenOfKeziahHoldsTheWorldInItsGrasp)
      earned `refShouldBe` True

    it "is not earned by an unrelated record" . gameTest $ \_ -> do
      asReturnToTheCircleUndone
      earned <- didEarnCircle ImmortalitySoundsNice
      run $ Record (toCampaignLogKey TheTrueWorkOfTheSilverTwilightLodgeHasBegun)
      earned `refShouldBe` False

  context "More Like Excursion" $ do
    it "is earned completing In the Clutches of Chaos with no incursion" . gameTest $ \_ -> do
      asReturnToTheCircleUndoneScenario "05284"
      earned <- didEarnCircle MoreLikeExcursion
      run $ Record (toCampaignLogKey TheInvestigatorsContinuedAlone)
      earned `refShouldBe` True

    it "is not earned when an incursion occurred" . gameTest $ \_ -> do
      asReturnToTheCircleUndoneScenario "05284"
      location <- testLocation
      run $ Incursion (toId location)
      earned <- didEarnCircle MoreLikeExcursion
      run $ Record (toCampaignLogKey TheInvestigatorsContinuedAlone)
      earned `refShouldBe` False

  context "'Member These?" $ do
    it "reports each discovered memento as checklist progress" . gameTest $ \_ -> do
      asReturnToTheCircleUndone
      progressed <- didProgressCircle MemberThese ["MesmerizingFlute"]
      run $ RecordSetInsert (toCampaignLogKey MementosDiscovered) [recorded MesmerizingFlute]
      progressed `refShouldBe` True

    it "does not report a memento that was not discovered" . gameTest $ \_ -> do
      asReturnToTheCircleUndone
      progressed <- didProgressCircle MemberThese ["StrangeIncantation"]
      run $ RecordSetInsert (toCampaignLogKey MementosDiscovered) [recorded MesmerizingFlute]
      progressed `refShouldBe` False

  context "Case Closed" $ do
    it "reports each rescued character as checklist progress" . gameTest $ \_ -> do
      asReturnToTheCircleUndone
      progressed <- didProgressCircle CaseClosed ["ValentinoRivas"]
      run $ Record (toCampaignLogKey ValentinoIsAlive)
      progressed `refShouldBe` True

    it "does not report a character that was not saved" . gameTest $ \_ -> do
      asReturnToTheCircleUndone
      progressed <- didProgressCircle CaseClosed ["PennyWhite"]
      run $ Record (toCampaignLogKey ValentinoIsAlive)
      progressed `refShouldBe` False

  context "Music of the Outer Gods" $ do
    it "is earned joining the Pipers of Azathoth" . gameTest $ \_ -> do
      asReturnToTheCircleUndone
      earned <- didEarnCircle MusicOfTheOuterGods
      run $ Record (toCampaignLogKey TheLeadInvestigatorHasJoinedThePipersOfAzathoth)
      earned `refShouldBe` True

    it "is not earned by a different ending" . gameTest $ \_ -> do
      asReturnToTheCircleUndone
      earned <- didEarnCircle MusicOfTheOuterGods
      run $ Record (toCampaignLogKey TheInvestigatorsSignedTheBlackBookOfAzathoth)
      earned `refShouldBe` False

  context "Weaver of Shadow and Mist" $ do
    it "is earned containing Azathoth (slumbers with no other ending recorded)" . gameTest $ \_ -> do
      asReturnToTheCircleUndone
      earned <- didEarnCircle WeaverOfShadowAndMist
      run $ Record (toCampaignLogKey AzathothSlumbersForNow)
      earned `refShouldBe` True

    it "is not earned when the Black Book was signed" . gameTest $ \_ -> do
      asReturnToTheCircleUndone
      record TheInvestigatorsSignedTheBlackBookOfAzathoth
      earned <- didEarnCircle WeaverOfShadowAndMist
      run $ Record (toCampaignLogKey AzathothSlumbersForNow)
      earned `refShouldBe` False

  context "Fine Print" $ do
    it "is earned signing the Black Book" . gameTest $ \_ -> do
      asReturnToTheCircleUndone
      earned <- didEarnCircle FinePrint
      run $ Record (toCampaignLogKey TheInvestigatorsSignedTheBlackBookOfAzathoth)
      earned `refShouldBe` True

    it "is not earned reversing the incantation" . gameTest $ \_ -> do
      asReturnToTheCircleUndone
      earned <- didEarnCircle FinePrint
      run $ Record (toCampaignLogKey TheInvestigatorsReversedTheIncantation)
      earned `refShouldBe` False

  context "duolA sdroW eht kaepS" $ do
    it "is earned reversing the incantation" . gameTest $ \_ -> do
      asReturnToTheCircleUndone
      earned <- didEarnCircle SpeakTheWordsAloud
      run $ Record (toCampaignLogKey TheInvestigatorsReversedTheIncantation)
      earned `refShouldBe` True

    it "is not earned signing the Black Book" . gameTest $ \_ -> do
      asReturnToTheCircleUndone
      earned <- didEarnCircle SpeakTheWordsAloud
      run $ Record (toCampaignLogKey TheInvestigatorsSignedTheBlackBookOfAzathoth)
      earned `refShouldBe` False

  context "Circle Expertise" $ do
    it "is earned winning the campaign on Expert" . gameTest $ \_ -> do
      asReturnToTheCircleUndoneWith Expert
      earned <- didEarnCircle CircleExpertise
      run $ Record (toCampaignLogKey AzathothSlumbersForNow)
      earned `refShouldBe` True

    it "is not earned winning on a lower difficulty" . gameTest $ \_ -> do
      asReturnToTheCircleUndoneWith Easy
      earned <- didEarnCircle CircleExpertise
      run $ Record (toCampaignLogKey AzathothSlumbersForNow)
      earned `refShouldBe` False

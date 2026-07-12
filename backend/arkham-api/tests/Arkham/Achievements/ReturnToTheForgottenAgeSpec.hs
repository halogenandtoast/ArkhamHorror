{- | Return to The Forgotten Age achievement detection.

The detections live on the campaign entity, so most specs drive them with the
same messages the real scenarios emit (campaign log records, enemy defeats,
damage assignment, supply purchases) rather than replaying whole scenarios.

"Scenario 5-What?" is the one that has to drive a real scenario message: the
skip has no record, only a Part 1 setup short-circuit, so its spec pushes the
'Setup' message with 6 paths known.
-}
module Arkham.Achievements.ReturnToTheForgottenAgeSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey (toCampaignLogKey)
import Arkham.Campaigns.TheForgottenAge.Key
import Arkham.Campaigns.TheForgottenAge.Supply (Supply (..))
import Arkham.DamageEffect (attack)
import Arkham.Difficulty
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Message
import Arkham.ScenarioLogKey
import Arkham.Source
import Arkham.Trait (Trait (Serpent))
import Arkham.Treachery.Cards qualified as Treacheries
import Helpers.Achievements
import TestImport.New

-- Shattered Aeons Resolution 1 (mended the tear) is a surviving campaign win.
winTheCampaign :: TestAppT ()
winTheCampaign = run $ Record (toCampaignLogKey TheInvestigatorsMendedTheTearInTheFabricOfTime)

spec :: Spec
spec = describe "Return to The Forgotten Age achievements" $ do
  context "Why Did It Have to Be Snakes?" $ do
    -- The Serpent trait comes from the Defeated message's trait list, so a
    -- vanilla test enemy is enough (the detection never queries the entity
    -- outside The Depths of Yoth).
    let killSerpent = do
          location <- testLocation
          serpent <- testEnemy
          serpent `spawnAt` location
          run $ Defeated (toTarget serpent) (toCardId serpent) (TestSource mempty) [Serpent]
    it "is earned after twenty Serpent defeats" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      earned <- didEarnForgottenAge WhyDidItHaveToBeSnakes
      replicateM_ 19 killSerpent
      earned `refShouldBe` False
      killSerpent
      earned `refShouldBe` True

    it "is not earned after nineteen" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      earned <- didEarnForgottenAge WhyDidItHaveToBeSnakes
      replicateM_ 19 killSerpent
      earned `refShouldBe` False

  context "Patricide" $ do
    it "is earned defeating Yig in The Depths of Yoth" . gameTest $ \_ -> do
      asReturnToTheForgottenAgeScenario "53059"
      location <- testLocation
      yig <- testEnemyWithDef Enemies.yig id
      yig `spawnAt` location
      earned <- didEarnForgottenAge Patricide
      run $ Defeated (toTarget yig) (toCardId yig) (TestSource mempty) [Serpent]
      earned `refShouldBe` True

    it "is not earned defeating Yig outside The Depths of Yoth" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      location <- testLocation
      yig <- testEnemyWithDef Enemies.yig id
      yig `spawnAt` location
      earned <- didEarnForgottenAge Patricide
      run $ Defeated (toTarget yig) (toCardId yig) (TestSource mempty) [Serpent]
      earned `refShouldBe` False

  context "Watch Them Unravel" $ do
    it "is earned when all four Threads of Fate act decks complete" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      earned <- didEarnForgottenAge WatchThemUnravel
      record TheInvestigatorsFoundTheMissingRelic
      record TheInvestigatorsRescuedAlejandro
      record TheInvestigatorsForgedABondWithIchtaca
      run $ Record (toCampaignLogKey TheInvestigatorsRecruitedTheHelpOfAnotherExpedition)
      earned `refShouldBe` True

    it "is not earned when a thread was left incomplete" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      earned <- didEarnForgottenAge WatchThemUnravel
      record TheInvestigatorsFoundTheMissingRelic
      record TheInvestigatorsRescuedAlejandro
      run $ Record (toCampaignLogKey TheInvestigatorsRecruitedTheHelpOfAnotherExpedition)
      earned `refShouldBe` False

  context "Hope for Humanity" $ do
    it "is earned when Ichtaca's faith is restored" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      earned <- didEarnForgottenAge HopeForHumanity
      run $ Record (toCampaignLogKey IchtacasFaithIsRestored)
      earned `refShouldBe` True

    it "is not earned by an unrelated record" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      earned <- didEarnForgottenAge HopeForHumanity
      run $ Record (toCampaignLogKey IchtacaIsInTheDark)
      earned `refShouldBe` False

  context "I Remember Everything!" $ do
    it "is earned when Alejandro's memories are restored" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      earned <- didEarnForgottenAge IRememberEverything
      run $ Record (toCampaignLogKey AlejandroRemembersEverything)
      earned `refShouldBe` True

    it "is not earned when Alejandro is set against you" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      earned <- didEarnForgottenAge IRememberEverything
      run $ Record (toCampaignLogKey AlejandroIsSetAgainstYou)
      earned `refShouldBe` False

  context "Beyond Perfection" $ do
    let rememberAll =
          for_
            [ FoundTheProcess
            , DissectedAnOrgan
            , InterviewedASubject
            , RealizedWhatYearItIs
            , ActivatedTheDevice
            , ReadAboutEarth
            , SawAFamiliarSpecimen
            ]
            (run . Remember)
    it "is earned completing all eight tasks" . gameTest $ \self -> do
      asReturnToTheForgottenAgeScenario "53053"
      earned <- didEarnForgottenAge BeyondPerfection
      rememberAll
      _ <- self `putAssetIntoPlay` Assets.theCustodian
      run $ Record (toCampaignLogKey TheProcessWasPerfected)
      earned `refShouldBe` True

    it "is not earned with only seven tasks" . gameTest $ \_ -> do
      asReturnToTheForgottenAgeScenario "53053"
      earned <- didEarnForgottenAge BeyondPerfection
      rememberAll
      run $ Record (toCampaignLogKey TheProcessWasPerfected)
      earned `refShouldBe` False

  context "He's Got a Point / Valusia Sounds Great" $ do
    it "earns He's Got a Point siding with Alejandro" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      earned <- didEarnForgottenAge HesGotAPoint
      run $ Record (toCampaignLogKey TheInvestigatorsSavedTheCivilizationOfTheYithians)
      earned `refShouldBe` True

    it "earns Valusia Sounds Great siding with Ichtaca" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      earned <- didEarnForgottenAge ValusiaSoundsGreat
      run $ Record (toCampaignLogKey TheInvestigatorsSavedTheCivilizationOfTheSerpents)
      earned `refShouldBe` True

    it "does not cross the wires between the two sides" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      earned <- didEarnForgottenAge HesGotAPoint
      run $ Record (toCampaignLogKey TheInvestigatorsSavedTheCivilizationOfTheSerpents)
      earned `refShouldBe` False

  context "Scenario 5-What?" $ do
    it "is earned skipping Heart of the Elders, Part 1" . gameTest $ \_ -> do
      asReturnToTheForgottenAgeScenario "53045"
      recordCount PathsAreKnownToYou 6
      earned <- didEarnForgottenAge Scenario5What
      run Setup
      earned `refShouldBe` True

    it "is not earned when Part 1 must be played" . gameTest $ \_ -> do
      asReturnToTheForgottenAgeScenario "53045"
      recordCount PathsAreKnownToYou 5
      earned <- didEarnForgottenAge Scenario5What
      run Setup
      earned `refShouldBe` False

  context "winning the campaign" $ do
    it "earns Yoth Expertise on Expert" . gameTest $ \_ -> do
      asReturnToTheForgottenAgeWith Expert
      earned <- didEarnForgottenAge YothExpertise
      winTheCampaign
      earned `refShouldBe` True

    it "does not earn Yoth Expertise below Expert" . gameTest $ \_ -> do
      asReturnToTheForgottenAgeWith Hard
      earned <- didEarnForgottenAge YothExpertise
      winTheCampaign
      earned `refShouldBe` False

    it "earns Don't Tread on Me with no Yig's Fury" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      recordCount YigsFury 0
      earned <- didEarnForgottenAge DontTreadOnMe
      winTheCampaign
      earned `refShouldBe` True

    it "does not earn Don't Tread on Me with Yig's Fury" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      recordCount YigsFury 3
      earned <- didEarnForgottenAge DontTreadOnMe
      winTheCampaign
      earned `refShouldBe` False

    it "earns Bane of Yig with 25 or more Yig's Fury" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      recordCount YigsFury 25
      earned <- didEarnForgottenAge BaneOfYig
      winTheCampaign
      earned `refShouldBe` True

    it "does not earn Bane of Yig below 25" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      recordCount YigsFury 24
      earned <- didEarnForgottenAge BaneOfYig
      winTheCampaign
      earned `refShouldBe` False

    it "earns I've Built Up An Immunity when never poisoned" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      earned <- didEarnForgottenAge IveBuiltUpAnImmunity
      winTheCampaign
      earned `refShouldBe` True

    it "does not earn I've Built Up An Immunity after becoming poisoned" . gameTest $ \self -> do
      asReturnToTheForgottenAge
      earned <- didEarnForgottenAge IveBuiltUpAnImmunity
      poison <- genCard Treacheries.poisoned
      run $ CreateWeaknessInThreatArea poison (toId self)
      winTheCampaign
      earned `refShouldBe` False

    it "earns We Have an Understanding when the Harbinger is unharmed" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      earned <- didEarnForgottenAge WeHaveAnUnderstanding
      winTheCampaign
      earned `refShouldBe` True

    it "does not earn We Have an Understanding after damaging the Harbinger" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      location <- testLocation
      harbinger <- testEnemyWithDef Enemies.harbingerOfValusia id
      harbinger `spawnAt` location
      earned <- didEarnForgottenAge WeHaveAnUnderstanding
      run $ Damaged (toTarget harbinger) (attack (TestSource mempty) 1)
      winTheCampaign
      earned `refShouldBe` False

    it "still earns We Have an Understanding on a zero-damage assignment" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      location <- testLocation
      harbinger <- testEnemyWithDef Enemies.harbingerOfValusia id
      harbinger `spawnAt` location
      earned <- didEarnForgottenAge WeHaveAnUnderstanding
      run $ Damaged (toTarget harbinger) (attack (TestSource mempty) 0)
      winTheCampaign
      earned `refShouldBe` True

    it "earns Who Needs Any of This Junk? when no supply is purchased" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      earned <- didEarnForgottenAge WhoNeedsAnyOfThisJunk
      winTheCampaign
      earned `refShouldBe` True

    it "does not earn Who Needs Any of This Junk? after buying a supply" . gameTest $ \self -> do
      asReturnToTheForgottenAge
      earned <- didEarnForgottenAge WhoNeedsAnyOfThisJunk
      run $ PickSupply (toId self) Rope
      winTheCampaign
      earned `refShouldBe` False

    it "earns If I Could Turn Back Time forging your own path" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      record YouAreForgingYourOwnWay
      earned <- didEarnForgottenAge IfICouldTurnBackTime
      run $ Record (toCampaignLogKey TheInvestigatorsSealedTheRelicOfAgesForever)
      earned `refShouldBe` True

    -- Turn Back Time's setup crosses out every ordered key except the first
    -- three, so at the winning record YouAreForgingYourOwnWay is crossed out
    -- rather than an active record. It must still count.
    it "earns If I Could Turn Back Time when the forge-path record is crossed out" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      record YouAreForgingYourOwnWay
      run $ CrossOutRecord (toCampaignLogKey YouAreForgingYourOwnWay)
      earned <- didEarnForgottenAge IfICouldTurnBackTime
      run $ Record (toCampaignLogKey TheInvestigatorsSealedTheRelicOfAgesForever)
      earned `refShouldBe` True

    it "does not earn If I Could Turn Back Time without forging your own path" . gameTest $ \_ -> do
      asReturnToTheForgottenAge
      earned <- didEarnForgottenAge IfICouldTurnBackTime
      run $ Record (toCampaignLogKey TheInvestigatorsSealedTheRelicOfAgesForever)
      earned `refShouldBe` False

  context "campaign gating" $ do
    it "does not earn in the base Forgotten Age campaign" . gameTest $ \_ -> do
      asAchievementCampaign "04" Easy
      earned <- didEarnForgottenAge DontTreadOnMe
      recordCount YigsFury 0
      run $ Record (toCampaignLogKey TheInvestigatorsMendedTheTearInTheFabricOfTime)
      earned `refShouldBe` False

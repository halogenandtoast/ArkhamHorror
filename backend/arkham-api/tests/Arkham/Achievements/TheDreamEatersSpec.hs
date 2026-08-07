{- | The Dream-Eaters achievement detection (both printed lists).

The detections live on the campaign entity, so most specs drive them with the
same messages the real scenarios and interludes emit (campaign log records,
enemy defeats, act advances, 'EndOfGame') rather than replaying whole scenarios.
Scenario-scoped detections use 'asTheDreamEatersScenario', which swaps in a
Gathering-shaped shell carrying the real scenario id — the scenario's own
behaviour never runs, which is exactly why the detections key on messages rather
than on resolutions.

Both mini-campaigns share campaign id "06"; the Dream-Quest/Web-of-Dreams split
is a display grouping, so a Web of Dreams achievement is just as earnable from
this same harness.
-}
module Arkham.Achievements.TheDreamEatersSpec (spec) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign (lookupCampaign)
import Arkham.Campaign.Types (CampaignAttrs (campaignLog, campaignMeta))
import Arkham.CampaignLog (CampaignLog (campaignLogRecorded), mkCampaignLog)
import Arkham.CampaignLogKey (toCampaignLogKey)
import Arkham.CampaignStep (CampaignStep (EpilogueStep))
import Arkham.Campaigns.TheDreamEaters.Key
import Arkham.Campaigns.TheDreamEaters.Meta
import Arkham.Difficulty
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types qualified as Enemy
import Arkham.Game.Settings (settingsAchievementsEnabled)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Cards qualified as Locations
import Arkham.Placement
import Arkham.ScenarioLogKey (ScenarioCountKey (SignOfTheGods))
import Arkham.Source
import Arkham.Token qualified as Token
import Arkham.Trait qualified as Trait
import Helpers.Achievements
import Helpers.UltimatumsAndBoons (Ultimatum (..), withUltimatums)
import TestImport.New

beyondTheGatesOfSleep, wakingNightmare, theSearchForKadath, aThousandShapesOfHorror :: CardCode
beyondTheGatesOfSleep = "06039"
wakingNightmare = "06063"
theSearchForKadath = "06119"
aThousandShapesOfHorror = "06168"

darkSideOfTheMoon, pointOfNoReturn, whereTheGodsDwell :: CardCode
darkSideOfTheMoon = "06206"
pointOfNoReturn = "06247"
whereTheGodsDwell = "06286"

-- | The message an act pushes as it advances (see 'advancedWithOther').
advanceActMsg :: CardDef -> Message
advanceActMsg def = AdvanceAct (ActId $ toCardCode def) (TestSource mempty) AdvancedWithOther

useAbilityOn :: Investigator -> Source -> Int -> TestAppT ()
useAbilityOn i source n = run $ UseCardAbility (toId i) source n [] NoPayment

defeatEnemyWith :: Source -> [Trait.Trait] -> Enemy.Enemy -> TestAppT ()
defeatEnemyWith source traits enemy = run $ Defeated (toTarget enemy) (toCardId enemy) source traits

spawnEnemy :: CardDef -> TestAppT Enemy.Enemy
spawnEnemy def = do
  location <- testLocation
  enemy <- testEnemyWithDef def id
  enemy `spawnAt` location
  pure enemy

{- | Put the campaign into the interconnected 8-part shape: The Dream-Quest is
the live half and The Web of Dreams rides in the metadata, which is how the
epilogue reads both halves' logs.
-}
asFullDreamEaters :: [TheDreamEatersKey] -> TestAppT ()
asFullDreamEaters webKeys = do
  asTheDreamEaters
  let
    webLog = mkCampaignLog {campaignLogRecorded = setFromList (map toCampaignLogKey webKeys)}
    webAttrs = (toAttrs $ lookupCampaign "06" Easy) {campaignLog = webLog}
    meta = Metadata FullMode (Just TheDreamQuest) (Just webAttrs) mempty mempty
  overTest \g ->
    g
      { gameMode = case gameMode g of
          These c s -> These (overAttrs (\a -> a {campaignMeta = toJSON meta}) c) s
          other -> other
      }
  tick

spec :: Spec
spec = describe "The Dream-Eaters achievements" $ do
  {- The campaign log's achievements tab shows only the mini-campaign being
  played, which it works out by reading `campaignMode` straight out of the
  untyped `campaign.meta` JSON. Nothing on the frontend type-checks that shape
  (Campaign.ts has `meta: any`), so pin it here. -}
  context "campaignMode meta encoding (read by the campaign log UI)" $ do
    it
      "tags a partial campaign with its part"
      ( toJSON (PartialMode TheWebOfDreams)
          `shouldBe` object ["tag" .= ("PartialMode" :: Text), "contents" .= ("TheWebOfDreams" :: Text)]
          :: IO ()
      )

    it
      "tags the full campaign with no contents"
      (toJSON FullMode `shouldBe` object ["tag" .= ("FullMode" :: Text)] :: IO ())

  ---------------------------------------------------------------------------
  -- The Dream-Quest
  ---------------------------------------------------------------------------

  context "Do You Always Follow Orders?" $ do
    it "is earned finishing Beyond the Gates of Sleep on the path" . gameTest $ \_ -> do
      asTheDreamEatersScenario beyondTheGatesOfSleep
      earned <- didEarnDreamQuest DoYouAlwaysFollowOrders
      run $ EndOfGame Nothing
      earned `refShouldBe` True

    it "is not earned once the dreamers strayed" . gameTest $ \_ -> do
      asTheDreamEatersScenario beyondTheGatesOfSleep
      record TheDreamersStrayedFromThePath
      earned <- didEarnDreamQuest DoYouAlwaysFollowOrders
      run $ EndOfGame Nothing
      earned `refShouldBe` False

    it "is not earned while achievements are disabled" . gameTest $ \_ -> do
      asTheDreamEatersScenario beyondTheGatesOfSleep
      overTest \g -> g {gameSettings = (gameSettings g) {settingsAchievementsEnabled = False}}
      earned <- didEarnDreamQuest DoYouAlwaysFollowOrders
      run $ EndOfGame Nothing
      earned `refShouldBe` False

  context "Aww, But They're So Cute" $ do
    it "is earned finishing the scenario with no Zoog defeated" . gameTest $ \_ -> do
      asTheDreamEatersScenario beyondTheGatesOfSleep
      earned <- didEarnDreamQuest AwwButTheyreSoCute
      run $ EndOfGame Nothing
      earned `refShouldBe` True

    it "is not earned after defeating a Zoog" . gameTest $ \_ -> do
      asTheDreamEatersScenario beyondTheGatesOfSleep
      zoog <- spawnEnemy Enemies.furtiveZoog
      earned <- didEarnDreamQuest AwwButTheyreSoCute
      defeatEnemyWith (TestSource mempty) [Trait.Creature, Trait.Zoog] zoog
      run $ EndOfGame Nothing
      earned `refShouldBe` False

    it "is still earned after defeating something that is not a Zoog" . gameTest $ \_ -> do
      asTheDreamEatersScenario beyondTheGatesOfSleep
      enemy <- spawnEnemy Enemies.nightriders
      earned <- didEarnDreamQuest AwwButTheyreSoCute
      defeatEnemyWith (TestSource mempty) [Trait.Monster] enemy
      run $ EndOfGame Nothing
      earned `refShouldBe` True

  context "Losing My Religion" $ do
    it "is earned uncovering all ten Signs of the Gods" . gameTest $ \_ -> do
      asTheDreamEatersScenario theSearchForKadath
      earned <- didEarnDreamQuest LosingMyReligion
      run $ ScenarioCountIncrementBy SignOfTheGods 9
      earned `refShouldBe` False
      run $ ScenarioCountIncrementBy SignOfTheGods 1
      earned `refShouldBe` True

    it "is not earned at nine" . gameTest $ \_ -> do
      asTheDreamEatersScenario theSearchForKadath
      earned <- didEarnDreamQuest LosingMyReligion
      run $ ScenarioCountIncrementBy SignOfTheGods 9
      earned `refShouldBe` False

  context "Fantasy Flight Games (R) Does Not Condone Accomplishing This Achievement" $ do
    it "is earned breaking the law of Ulthar" . gameTest $ \self -> do
      asTheDreamEatersScenario theSearchForKadath
      earned <- didEarnDreamQuest FantasyFlightGamesDoesNotCondoneAccomplishingThisAchievement
      run $ RecordForInvestigator (toId self) (toCampaignLogKey HasBrokenTheLawOfUlthar)
      earned `refShouldBe` True

    it "is not earned by other per-investigator records" . gameTest $ \self -> do
      asTheDreamEatersScenario theSearchForKadath
      earned <- didEarnDreamQuest FantasyFlightGamesDoesNotCondoneAccomplishingThisAchievement
      run $ RecordForInvestigator (toId self) (toCampaignLogKey WasCaptured)
      earned `refShouldBe` False

  context "Tactical Espionage Action" $ do
    it "is earned completing the scenario at alarm level zero" . gameTest $ \_ -> do
      asTheDreamEatersScenario darkSideOfTheMoon
      earned <- didEarnDreamQuest TacticalEspionageAction
      run $ EndOfGame Nothing
      earned `refShouldBe` True

    it "is not earned with an alarm token still on an investigator" . gameTest $ \self -> do
      asTheDreamEatersScenario darkSideOfTheMoon
      run $ PlaceTokens (TestSource mempty) (toTarget self) Token.AlarmLevel 1
      earned <- didEarnDreamQuest TacticalEspionageAction
      run $ EndOfGame Nothing
      earned `refShouldBe` False

    -- Alarm levels survive resigning, so the check must include eliminated
    -- investigators rather than only the ones still standing.
    it "is not earned when a resigned investigator kept their alarm" . gameTest $ \self -> do
      asTheDreamEatersScenario darkSideOfTheMoon
      other <- addInvestigator Investigators.rolandBanks
      run $ PlaceTokens (TestSource mempty) (toTarget other) Token.AlarmLevel 1
      run $ Resign (toId other)
      _ <- pure self
      earned <- didEarnDreamQuest TacticalEspionageAction
      run $ EndOfGame Nothing
      earned `refShouldBe` False

  context "Moon Lizards? I Don't Believe They Exist" $ do
    it "is earned defeating the Moon Lizard" . gameTest $ \_ -> do
      asTheDreamEatersScenario darkSideOfTheMoon
      lizard <- spawnEnemy Enemies.moonLizard
      earned <- didEarnDreamQuest MoonLizardsIDontBelieveTheyExist
      defeatEnemyWith (TestSource mempty) [] lizard
      earned `refShouldBe` True

    it "is not earned defeating anything else" . gameTest $ \_ -> do
      asTheDreamEatersScenario darkSideOfTheMoon
      beast <- spawnEnemy Enemies.moonBeast
      earned <- didEarnDreamQuest MoonLizardsIDontBelieveTheyExist
      defeatEnemyWith (TestSource mempty) [] beast
      earned `refShouldBe` False

  context "Barkham Horror Enthusiast" $ do
    -- Cat and dog are not printed traits; the allies carry a "cat"/"dog" tag.
    let killCatsWith source = do
          cats <- spawnEnemy Enemies.catsFromSaturn
          defeatEnemyWith source [] cats

    it "is earned when a dog ally's fight ability lands the kill" . gameTest $ \self -> do
      asTheDreamEatersScenario darkSideOfTheMoon
      duke <- testAssetWithDef Assets.duke id self
      earned <- didEarnDreamQuest BarkhamHorrorEnthusiast
      killCatsWith (AbilitySource (AssetSource $ toId duke) 1)
      earned `refShouldBe` True

    it "is earned when a cat ally's fight ability lands the kill" . gameTest $ \self -> do
      asTheDreamEatersScenario darkSideOfTheMoon
      zeal <- testAssetWithDef Assets.zeal id self
      earned <- didEarnDreamQuest BarkhamHorrorEnthusiast
      killCatsWith (AbilitySource (AssetSource $ toId zeal) 1)
      earned `refShouldBe` True

    it "is not earned when another ally lands the kill" . gameTest $ \self -> do
      asTheDreamEatersScenario darkSideOfTheMoon
      other <- testAssetWithDef Assets.guardDog id self
      earned <- didEarnDreamQuest BarkhamHorrorEnthusiast
      killCatsWith (AbilitySource (AssetSource $ toId other) 1)
      earned `refShouldBe` False

    it "is not earned killing something other than Cats from Saturn" . gameTest $ \self -> do
      asTheDreamEatersScenario darkSideOfTheMoon
      duke <- testAssetWithDef Assets.duke id self
      beast <- spawnEnemy Enemies.moonBeast
      earned <- didEarnDreamQuest BarkhamHorrorEnthusiast
      defeatEnemyWith (AbilitySource (AssetSource $ toId duke) 1) [] beast
      earned `refShouldBe` False

  context "Only Way To Be Sure" $ do
    -- The High Priest has 3 health per investigator, so a solo test needs 2
    -- damage to leave exactly 1 remaining.
    let shoveHimDownTheWell i =
          useAbilityOn i (ActSource (ActId $ toCardCode Acts.theThingInTheRobes)) 1

    it "is earned shoving the priest down the well at 1 remaining health" . gameTest $ \self -> do
      asTheDreamEatersScenario whereTheGodsDwell
      priest <- spawnEnemy Enemies.highPriestNotToBeDescribed
      run $ PlaceDamage (TestSource mempty) (toTarget priest) 2
      earned <- didEarnDreamQuest OnlyWayToBeSure
      shoveHimDownTheWell self
      earned `refShouldBe` True

    it "is not earned while the priest has more health left" . gameTest $ \self -> do
      asTheDreamEatersScenario whereTheGodsDwell
      priest <- spawnEnemy Enemies.highPriestNotToBeDescribed
      run $ PlaceDamage (TestSource mempty) (toTarget priest) 1
      earned <- didEarnDreamQuest OnlyWayToBeSure
      shoveHimDownTheWell self
      earned `refShouldBe` False

  context "Give Them Something To Talk About" $ do
    -- Beyond Dreams keeps 1 + player count of the five forms, so a solo game
    -- needs two of them in the victory display in one round.
    let victoryForm def = do
          form <- spawnEnemy def
          run $ AddToVictory Nothing (toTarget form)

    it "is earned adding every form in a single round" . gameTest $ \_ -> do
      asTheDreamEatersScenario whereTheGodsDwell
      earned <- didEarnDreamQuest GiveThemSomethingToTalkAbout
      victoryForm Enemies.nyarlathotepTheCrawlingChaos
      earned `refShouldBe` False
      victoryForm Enemies.nyarlathotepTheFacelessWhisperer
      earned `refShouldBe` True

    it "is not earned when the forms straddle two rounds" . gameTest $ \_ -> do
      asTheDreamEatersScenario whereTheGodsDwell
      earned <- didEarnDreamQuest GiveThemSomethingToTalkAbout
      victoryForm Enemies.nyarlathotepTheCrawlingChaos
      run EndRound
      victoryForm Enemies.nyarlathotepTheFacelessWhisperer
      earned `refShouldBe` False

  context "This Isn't Even My Final Form!" $ do
    it "is earned defeating Nyarlathotep's True Shape" . gameTest $ \_ -> do
      asTheDreamEatersScenario whereTheGodsDwell
      trueShape <- spawnEnemy Enemies.nyarlathotepTrueShape
      earned <- didEarnDreamQuest ThisIsntEvenMyFinalForm
      defeatEnemyWith (TestSource mempty) [] trueShape
      earned `refShouldBe` True

    it "is not earned defeating a hidden form" . gameTest $ \_ -> do
      asTheDreamEatersScenario whereTheGodsDwell
      form <- spawnEnemy Enemies.nyarlathotepTheCrawlingChaos
      earned <- didEarnDreamQuest ThisIsntEvenMyFinalForm
      defeatEnemyWith (TestSource mempty) [] form
      earned `refShouldBe` False

  context "Don't Tell Anyone, But..." $ do
    {- The Great Hall's ability is the only way a hidden card moves between hands,
    which is what the detection reads: a HiddenInHand placement whose card is
    already hidden in a DIFFERENT hand. Drawing a hidden card has no prior
    placement, so it is not a transfer. -}
    let giveHiddenCards self other n =
          replicateM_ n do
            hidden <-
              testEnemyWithDef Enemies.nyarlathotepTheCrawlingChaos
                $ Enemy.placementL
                .~ HiddenInHand (toId self)
            run $ PlaceEnemy (toId hidden) (HiddenInHand $ toId other)

    it "is earned after six different hidden cards change hands" . gameTest $ \self -> do
      asTheDreamEatersScenario whereTheGodsDwell
      other <- addInvestigator Investigators.rolandBanks
      earned <- didEarnDreamQuest DontTellAnyoneBut
      giveHiddenCards self other 5
      earned `refShouldBe` False
      giveHiddenCards self other 1
      earned `refShouldBe` True

    it "is not earned after five" . gameTest $ \self -> do
      asTheDreamEatersScenario whereTheGodsDwell
      other <- addInvestigator Investigators.rolandBanks
      earned <- didEarnDreamQuest DontTellAnyoneBut
      giveHiddenCards self other 5
      earned `refShouldBe` False

    it "does not count a hidden card entering its own holder's hand" . gameTest $ \self -> do
      asTheDreamEatersScenario whereTheGodsDwell
      earned <- didEarnDreamQuest DontTellAnyoneBut
      replicateM_ 6 do
        hidden <- testEnemyWithDef Enemies.nyarlathotepTheCrawlingChaos id
        run $ PlaceEnemy (toId hidden) (HiddenInHand $ toId self)
      earned `refShouldBe` False

  context "winning The Dream-Quest" $ do
    -- Where the Gods Dwell's resolutions 3-5 are only reachable from its winning
    -- resolutions 1 and 2, so each of their records means the half was won.
    let winTheDreamQuest = record TheDreamersAwoke
        loseTheDreamQuest = record Nyarlathotep'sInvasionHasBegun

    it "earns Line in the Sand with three active ultimatums" . gameTest $ \_ -> do
      asTheDreamEaters
      withUltimatums [UltimatumOfFailure, UltimatumOfHardship, UltimatumOfDread]
      earned <- didEarnDreamQuest DreamQuestLineInTheSand
      winTheDreamQuest
      earned `refShouldBe` True

    it "does not earn Line in the Sand with only two" . gameTest $ \_ -> do
      asTheDreamEaters
      withUltimatums [UltimatumOfFailure, UltimatumOfHardship]
      earned <- didEarnDreamQuest DreamQuestLineInTheSand
      winTheDreamQuest
      earned `refShouldBe` False

    it "does not earn Line in the Sand when the campaign is lost" . gameTest $ \_ -> do
      asTheDreamEaters
      withUltimatums [UltimatumOfFailure, UltimatumOfHardship, UltimatumOfDread]
      earned <- didEarnDreamQuest DreamQuestLineInTheSand
      loseTheDreamQuest
      earned `refShouldBe` False

    it "earns Dreamlands Expertise on Expert" . gameTest $ \_ -> do
      asTheDreamEatersWith Expert
      earned <- didEarnDreamQuest DreamlandsExpertise
      winTheDreamQuest
      earned `refShouldBe` True

    it "does not earn Dreamlands Expertise below Expert" . gameTest $ \_ -> do
      asTheDreamEatersWith Hard
      earned <- didEarnDreamQuest DreamlandsExpertise
      winTheDreamQuest
      earned `refShouldBe` False

  context "Beware The Black Cat" $ do
    it "is earned completing the campaign having told the cat off" . gameTest $ \_ -> do
      asTheDreamEaters
      record OkayFineHaveItYourWayThen
      earned <- didEarnDreamQuest BewareTheBlackCat
      record TheDreamersAwoke
      earned `refShouldBe` True

    -- It only asks that the campaign be COMPLETED, so losing still counts.
    it "is earned even when the campaign is lost" . gameTest $ \_ -> do
      asTheDreamEaters
      record OkayFineHaveItYourWayThen
      earned <- didEarnDreamQuest BewareTheBlackCat
      record Nyarlathotep'sInvasionHasBegun
      earned `refShouldBe` True

    it "is not earned without the record" . gameTest $ \_ -> do
      asTheDreamEaters
      earned <- didEarnDreamQuest BewareTheBlackCat
      record TheDreamersAwoke
      earned `refShouldBe` False

  context "Reunited and it Feels So Good" $ do
    -- Epilogue 6: the dreamers awoke and the other group returned to reality.
    it "is earned when both groups reunite in the waking world" . gameTest $ \_ -> do
      asFullDreamEaters [TheInvestigatorsReturnedToReality]
      record TheDreamersAwoke
      earned <- didEarnDreamQuest ReunitedAndItFeelsSoGood
      run $ CampaignStep EpilogueStep
      earned `refShouldBe` True

    -- Epilogue 12: both groups stayed in the Dreamlands.
    it "is earned when both groups reunite in the Dreamlands" . gameTest $ \_ -> do
      asFullDreamEaters [TheInvestigatorsAreStillInTheDreamlands]
      record TheDreamersStayedInTheDreamlandsForever
      earned <- didEarnDreamQuest ReunitedAndItFeelsSoGood
      run $ CampaignStep EpilogueStep
      earned `refShouldBe` True

    -- Epilogue 8: both survived, but forever separated.
    it "is not earned when the groups stay separated" . gameTest $ \_ -> do
      asFullDreamEaters [TheInvestigatorsAreStillInTheDreamlands]
      record TheDreamersAwoke
      earned <- didEarnDreamQuest ReunitedAndItFeelsSoGood
      run $ CampaignStep EpilogueStep
      earned `refShouldBe` False

    -- Epilogue 2 reunites them in the waking world but is not a win.
    it "is not earned when Nyarlathotep's invasion has begun" . gameTest $ \_ -> do
      asFullDreamEaters [TheInvestigatorsReturnedToReality]
      record Nyarlathotep'sInvasionHasBegun
      earned <- didEarnDreamQuest ReunitedAndItFeelsSoGood
      run $ CampaignStep EpilogueStep
      earned `refShouldBe` False

  ---------------------------------------------------------------------------
  -- The Web of Dreams
  ---------------------------------------------------------------------------

  context "Everyone's a Feminist Until There Is a Spider Around" $ do
    let defeatSpider = do
          spider <- spawnEnemy Enemies.spiderOfLeng
          defeatEnemyWith (TestSource mempty) [Trait.Monster, Trait.Spider] spider

    it "is earned on the twentieth spider" . gameTest $ \_ -> do
      asTheDreamEaters
      earned <- didEarnWebOfDreams EveryonesAFeministUntilThereIsASpiderAround
      replicateM_ 19 defeatSpider
      earned `refShouldBe` False
      defeatSpider
      earned `refShouldBe` True

    it "is not earned after nineteen" . gameTest $ \_ -> do
      asTheDreamEaters
      earned <- didEarnWebOfDreams EveryonesAFeministUntilThereIsASpiderAround
      replicateM_ 19 defeatSpider
      earned `refShouldBe` False

    -- Swarm cards come off an investigator's deck, so the swarm card has to be
    -- an owned player card: removing a swarm enemy puts it back on that deck.
    it "does not count swarm cards" . gameTest $ \self -> do
      asTheDreamEaters
      host <- spawnEnemy Enemies.spiderOfLeng
      earned <- didEarnWebOfDreams EveryonesAFeministUntilThereIsASpiderAround
      replicateM_ 20 do
        swarmCard <- testPlayerCard id
        swarm <-
          testEnemyWithDef Enemies.pitchSpider
            $ Enemy.placementL
            .~ AsSwarm (toId host) (toCard swarmCard {pcOwner = Just (toId self)})
        defeatEnemyWith (TestSource mempty) [Trait.Monster, Trait.Spider] swarm
      earned `refShouldBe` False

  context "The Carter Method" $ do
    -- A location only gains a horror token by being sealed, and it can only be
    -- sealed after becoming infested.
    let containTheOutbreak = run $ advanceActMsg Acts.containingTheOutbreak

    it "is earned with every location sealed" . gameTest $ \_ -> do
      asTheDreamEatersScenario wakingNightmare
      location <- testLocation
      run $ PlaceHorror (TestSource mempty) (toTarget location) 1
      earned <- didEarnWebOfDreams TheCarterMethod
      containTheOutbreak
      earned `refShouldBe` True

    it "is not earned with an unsealed location left" . gameTest $ \_ -> do
      asTheDreamEatersScenario wakingNightmare
      location <- testLocation
      run $ PlaceHorror (TestSource mempty) (toTarget location) 1
      _ <- testLocation
      earned <- didEarnWebOfDreams TheCarterMethod
      containTheOutbreak
      earned `refShouldBe` False

    it "is not earned with no locations at all" . gameTest $ \_ -> do
      asTheDreamEatersScenario wakingNightmare
      earned <- didEarnWebOfDreams TheCarterMethod
      containTheOutbreak
      earned `refShouldBe` False

  context "The Doctor is In" $ do
    it "is earned when she came along and was never hurt" . gameTest $ \self -> do
      asTheDreamEatersScenario wakingNightmare
      record DrMaheswaranJoinedTheInvestigation
      _ <- testAssetWithDef Assets.drShivaniMaheswaran id self
      earned <- didEarnWebOfDreams TheDoctorIsIn
      run $ EndOfGame Nothing
      earned `refShouldBe` True

    it "is not earned once she takes damage" . gameTest $ \self -> do
      asTheDreamEatersScenario wakingNightmare
      record DrMaheswaranJoinedTheInvestigation
      doctor <- testAssetWithDef Assets.drShivaniMaheswaran id self
      earned <- didEarnWebOfDreams TheDoctorIsIn
      run $ PlaceDamage (TestSource mempty) (toTarget doctor) 1
      run $ EndOfGame Nothing
      earned `refShouldBe` False

    it "is not earned once she takes horror" . gameTest $ \self -> do
      asTheDreamEatersScenario wakingNightmare
      record DrMaheswaranJoinedTheInvestigation
      doctor <- testAssetWithDef Assets.drShivaniMaheswaran id self
      earned <- didEarnWebOfDreams TheDoctorIsIn
      run $ PlaceHorror (TestSource mempty) (toTarget doctor) 1
      run $ EndOfGame Nothing
      earned `refShouldBe` False

    it "is not earned when she stayed with her patients" . gameTest $ \_ -> do
      asTheDreamEatersScenario wakingNightmare
      record DrMaheswaranStayedWithHerPatients
      earned <- didEarnWebOfDreams TheDoctorIsIn
      run $ EndOfGame Nothing
      earned `refShouldBe` False

  context "Déjà Vu" $ do
    {- Every free triggered ability on every A Thousand Shapes of Horror location,
    as (location, ability). The Entryway/Library pair and the Parlor/Attic pair
    normally lock each other out; with achievements on both stay offerable. -}
    let allAbilities :: [(CardDef, Int)]
        allAbilities =
          [ (Locations.burialGround, 1)
          , (Locations.frontPorchEntryway, 1)
          , (Locations.frontPorchEntryway, 2)
          , (Locations.downstairsDoorwayDen, 2)
          , (Locations.downstairsDoorwayParlor, 1)
          , (Locations.upstairsHallway, 1)
          , (Locations.upstairsDoorwayLibrary, 1)
          , (Locations.upstairsDoorwayBedroom, 2)
          , (Locations.attic_AThousandShapesOfHorror, 1)
          ]
        {- These abilities run for real, so the house has to be standing: the
        Entryway reveals the Upstairs Hallway, and the Upstairs Hallway and the
        Entryway/Library pair pull the Attic and the Unmarked Tomb out of the
        set-aside pile. -}
        setUpTheHouse = do
          setAside <- traverse genCard [Locations.unmarkedTomb, Locations.attic_AThousandShapesOfHorror]
          run $ SetAsideCards setAside
          for (nub $ map fst allAbilities) \def -> do
            location <- testLocationWithDef def id
            pure (toCardCode def, toId location)
        resolveAbility i placed (def, n) =
          for_ (lookup (toCardCode def) placed) \lid -> useAbilityOn i (LocationSource lid) n
        (allButLastAbility, lastAbility) = splitAt (length allAbilities - 1 :: Int) allAbilities

    it "is earned once every free triggered ability has been resolved" . gameTest $ \self -> do
      asTheDreamEatersScenario aThousandShapesOfHorror
      placed <- setUpTheHouse
      earned <- didEarnWebOfDreams DejaVu
      traverse_ (resolveAbility self placed) allButLastAbility
      earned `refShouldBe` False
      traverse_ (resolveAbility self placed) lastAbility
      earned `refShouldBe` True

    it "is not earned with one ability left" . gameTest $ \self -> do
      asTheDreamEatersScenario aThousandShapesOfHorror
      placed <- setUpTheHouse
      earned <- didEarnWebOfDreams DejaVu
      traverse_ (resolveAbility self placed) allButLastAbility
      earned `refShouldBe` False

    it "does not count the same ability twice" . gameTest $ \self -> do
      asTheDreamEatersScenario aThousandShapesOfHorror
      placed <- setUpTheHouse
      earned <- didEarnWebOfDreams DejaVu
      replicateM_ 10 $ resolveAbility self placed (Locations.burialGround, 1)
      earned `refShouldBe` False

  context "The Casa Loma Maneuver" $ do
    let escapeTheStairs = run $ advanceActMsg Acts.theEndlessStairs
        placeUnnamableAt label = do
          stairs <- testLocationWithDef Locations.mysteriousStairs_183 id
          run $ SetLocationLabel (toId stairs) label
          unnamable <- testEnemyWithDef Enemies.theUnnamable id
          unnamable `spawnAt` stairs

    it "is earned with The Unnamable at the topmost staircase" . gameTest $ \_ -> do
      asTheDreamEatersScenario aThousandShapesOfHorror
      placeUnnamableAt "mysteriousStairs1"
      earned <- didEarnWebOfDreams TheCasaLomaManeuver
      escapeTheStairs
      earned `refShouldBe` True

    it "is not earned with The Unnamable further down" . gameTest $ \_ -> do
      asTheDreamEatersScenario aThousandShapesOfHorror
      placeUnnamableAt "mysteriousStairs3"
      earned <- didEarnWebOfDreams TheCasaLomaManeuver
      escapeTheStairs
      earned `refShouldBe` False

  context "I Remember This Place" $ do
    it "is earned finding a way out of the Underworld" . gameTest $ \_ -> do
      asTheDreamEatersScenario pointOfNoReturn
      earned <- didEarnWebOfDreams IRememberThisPlace
      record TheInvestigatorsFoundAWayOutOfTheUnderworld
      earned `refShouldBe` True

    it "is not earned by other records" . gameTest $ \_ -> do
      asTheDreamEatersScenario pointOfNoReturn
      earned <- didEarnWebOfDreams IRememberThisPlace
      record RandolphDidNotSurviveTheDescent
      earned `refShouldBe` False

  context "Bad Advice" $ do
    let flippableLocations =
          [ Locations.vaultsOfZin
          , Locations.cityOfGugs
          , Locations.towerOfKoth
          , Locations.plainOfTheGhouls
          , Locations.cragOfTheGhouls
          , Locations.seaOfBones
          , Locations.peaksOfThok
          , Locations.valeOfPnath
          , Locations.seaOfPitch_262
          , Locations.seaOfPitch_263
          , Locations.seaOfPitch_264
          , Locations.seaOfPitch_265
          ]
        flipLocation i def = do
          location <- testLocationWithDef def id
          run $ Flip (toId i) (TestSource mempty) (toTarget location)
        (allButLastLocation, lastLocation) = splitAt (length flippableLocations - 1 :: Int) flippableLocations

    it "is earned once every location has been flipped" . gameTest $ \self -> do
      asTheDreamEatersScenario pointOfNoReturn
      earned <- didEarnWebOfDreams BadAdvice
      traverse_ (flipLocation self) allButLastLocation
      earned `refShouldBe` False
      traverse_ (flipLocation self) lastLocation
      earned `refShouldBe` True

    it "is not earned with one location left" . gameTest $ \self -> do
      asTheDreamEatersScenario pointOfNoReturn
      earned <- didEarnWebOfDreams BadAdvice
      traverse_ (flipLocation self) allButLastLocation
      earned `refShouldBe` False

  context "March of the Ghouls" $ do
    let attachGhouls aid n = do
          cards <- replicateM n (genCard Enemies.ghoulMinion)
          run $ PlaceUnderneath (AssetTarget aid) cards

    it "is earned with four Ghouls attached to Pickman" . gameTest $ \self -> do
      asTheDreamEatersScenario pointOfNoReturn
      pickman <- testAssetWithDef Assets.richardUptonPickman id self
      earned <- didEarnWebOfDreams MarchOfTheGhouls
      attachGhouls (toId pickman) 3
      earned `refShouldBe` False
      attachGhouls (toId pickman) 1
      earned `refShouldBe` True

    it "is not earned with three" . gameTest $ \self -> do
      asTheDreamEatersScenario pointOfNoReturn
      pickman <- testAssetWithDef Assets.richardUptonPickman id self
      earned <- didEarnWebOfDreams MarchOfTheGhouls
      attachGhouls (toId pickman) 3
      earned `refShouldBe` False

    it "does not count non-Ghoul cards" . gameTest $ \self -> do
      asTheDreamEatersScenario pointOfNoReturn
      pickman <- testAssetWithDef Assets.richardUptonPickman id self
      earned <- didEarnWebOfDreams MarchOfTheGhouls
      cards <- replicateM 4 (genCard Enemies.gugSentinel)
      run $ PlaceUnderneath (AssetTarget $ toId pickman) cards
      earned `refShouldBe` False

  context "The Ishimura Flex" $ do
    let legs =
          [ Enemies.legsOfAtlachNacha_347
          , Enemies.legsOfAtlachNacha_348
          , Enemies.legsOfAtlachNacha_349
          , Enemies.legsOfAtlachNacha_350
          ]
        defeatLeg def = do
          leg <- spawnEnemy def
          defeatEnemyWith (TestSource mempty) [] leg
        (allButLastLeg, lastLeg) = splitAt (length legs - 1 :: Int) legs

    it "is earned defeating all four legs in one round" . gameTest $ \_ -> do
      asTheDreamEaters
      earned <- didEarnWebOfDreams TheIshimuraFlex
      traverse_ defeatLeg allButLastLeg
      earned `refShouldBe` False
      traverse_ defeatLeg lastLeg
      earned `refShouldBe` True

    it "is not earned when the round turns over first" . gameTest $ \_ -> do
      asTheDreamEaters
      earned <- didEarnWebOfDreams TheIshimuraFlex
      traverse_ defeatLeg allButLastLeg
      run EndRound
      traverse_ defeatLeg lastLeg
      earned `refShouldBe` False

  context "You Spin Me Right 'Round" $ do
    let spin i eid n = run $ HandleAbilityOption (toId i) (EnemySource eid) n

    it "is earned on a full 360 degree turn in one phase" . gameTest $ \self -> do
      asTheDreamEaters
      atlachNacha <- spawnEnemy Enemies.atlachNacha
      earned <- didEarnWebOfDreams YouSpinMeRightRound
      spin self (toId atlachNacha) 7
      earned `refShouldBe` False
      spin self (toId atlachNacha) 1
      earned `refShouldBe` True

    it "is not earned at 315 degrees" . gameTest $ \self -> do
      asTheDreamEaters
      atlachNacha <- spawnEnemy Enemies.atlachNacha
      earned <- didEarnWebOfDreams YouSpinMeRightRound
      spin self (toId atlachNacha) 7
      earned `refShouldBe` False

    it "is not earned when the spin straddles two phases" . gameTest $ \self -> do
      asTheDreamEaters
      atlachNacha <- spawnEnemy Enemies.atlachNacha
      earned <- didEarnWebOfDreams YouSpinMeRightRound
      spin self (toId atlachNacha) 7
      run EndPhase
      spin self (toId atlachNacha) 1
      earned `refShouldBe` False

  context "Master of Unlocking" $ do
    it "is earned on the tenth horror canceled in a scenario" . gameTest $ \self -> do
      asTheDreamEatersScenario aThousandShapesOfHorror
      key <- testAssetWithDef Assets.theSilverKey id self
      earned <- didEarnWebOfDreams MasterOfUnlocking
      replicateM_ 9 $ useAbilityOn self (AssetSource $ toId key) 1
      earned `refShouldBe` False
      useAbilityOn self (AssetSource $ toId key) 1
      earned `refShouldBe` True

    it "is not earned after nine" . gameTest $ \self -> do
      asTheDreamEatersScenario aThousandShapesOfHorror
      key <- testAssetWithDef Assets.theSilverKey id self
      earned <- didEarnWebOfDreams MasterOfUnlocking
      replicateM_ 9 $ useAbilityOn self (AssetSource $ toId key) 1
      earned `refShouldBe` False

    it "does not count other assets' abilities" . gameTest $ \self -> do
      asTheDreamEatersScenario aThousandShapesOfHorror
      other <- testAsset id self
      earned <- didEarnWebOfDreams MasterOfUnlocking
      replicateM_ 10 $ useAbilityOn self (AssetSource $ toId other) 1
      earned `refShouldBe` False

  context "winning The Web of Dreams" $ do
    -- Weaver of the Cosmos' resolutions 3-5 are only reachable from its winning
    -- resolution 1, so each of their records means the half was won.
    let winTheWebOfDreams = record TheInvestigatorsReturnedToReality
        loseTheWebOfDreams = record TheBridgeWasCompleted

    it "earns Line in the Sand with three active ultimatums" . gameTest $ \_ -> do
      asTheDreamEaters
      withUltimatums [UltimatumOfFailure, UltimatumOfHardship, UltimatumOfDread]
      earned <- didEarnWebOfDreams WebOfDreamsLineInTheSand
      winTheWebOfDreams
      earned `refShouldBe` True

    it "does not earn Line in the Sand with only two" . gameTest $ \_ -> do
      asTheDreamEaters
      withUltimatums [UltimatumOfFailure, UltimatumOfHardship]
      earned <- didEarnWebOfDreams WebOfDreamsLineInTheSand
      winTheWebOfDreams
      earned `refShouldBe` False

    it "does not earn Line in the Sand when the campaign is lost" . gameTest $ \_ -> do
      asTheDreamEaters
      withUltimatums [UltimatumOfFailure, UltimatumOfHardship, UltimatumOfDread]
      earned <- didEarnWebOfDreams WebOfDreamsLineInTheSand
      loseTheWebOfDreams
      earned `refShouldBe` False

    it "earns Underworld Expertise on Expert" . gameTest $ \_ -> do
      asTheDreamEatersWith Expert
      earned <- didEarnWebOfDreams UnderworldExpertise
      winTheWebOfDreams
      earned `refShouldBe` True

    it "does not earn Underworld Expertise below Expert" . gameTest $ \_ -> do
      asTheDreamEatersWith Hard
      earned <- didEarnWebOfDreams UnderworldExpertise
      winTheWebOfDreams
      earned `refShouldBe` False

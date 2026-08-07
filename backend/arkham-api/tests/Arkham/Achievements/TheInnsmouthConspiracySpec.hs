{- | The Innsmouth Conspiracy achievement detection.

The detections live on the campaign entity, so most specs drive them with the
same messages the real scenarios and interludes emit (enemy defeats, campaign
log records, scenario-count changes, 'EndOfGame') rather than replaying whole
scenarios. Scenario-scoped detections use 'asTheInnsmouthConspiracyScenario',
which swaps in a Gathering-shaped shell carrying the real scenario id — the
scenario's own behaviour never runs, which is exactly why the detections key on
messages rather than on resolutions.

'atScenario' below is the one addition to that pattern: it swaps only the
scenario, leaving the campaign (and so its achievement store) in place, which is
what "Don't Wake Daddy" needs to span The Lair of Dagon and Into the Maelstrom.
-}
module Arkham.Achievements.TheInnsmouthConspiracySpec (spec) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types qualified as Asset
import Arkham.CampaignLogKey (recorded, toCampaignLogKey)
import Arkham.CampaignStep (CampaignStep (EpilogueStep))
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.Difficulty
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Settings (settingsAchievementsEnabled)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Cards qualified as Locations
import Arkham.Placement
import Arkham.ScenarioLogKey (ScenarioCountKey (Barriers))
import Arkham.Scenarios.InTooDeep.Helpers qualified as InTooDeep
import Arkham.SortedPair (sortedPair)
import Arkham.Trait (Trait (DeepOne))
import Data.Map.Strict qualified as Map
import Helpers.Achievements
import Helpers.UltimatumsAndBoons (Ultimatum (..), withUltimatums)
import TestImport.New

-- | Every surviving ending of the campaign routes through the epilogue.
finishTheCampaign :: TestAppT ()
finishTheCampaign = run $ CampaignStep EpilogueStep

{- | Swap the harness scenario without rebuilding the campaign, so achievement
store writes made during an earlier scenario survive into the next one.
'asTheInnsmouthConspiracyScenario' replaces the campaign too, which would wipe
them.
-}
atScenario :: CardCode -> TestAppT ()
atScenario code = do
  scenario' <- testScenario code id
  overTest \g ->
    g
      { gameMode =
          These
            (fromJustNote "campaign attached by asTheInnsmouthConspiracy" $ modeCampaign (gameMode g))
            scenario'
      }
  tick

-- | The message an act pushes as it advances (see 'advancedWithOther').
advanceActMsg :: CardDef -> Message
advanceActMsg def = AdvanceAct (ActId $ toCardCode def) (TestSource mempty) AdvancedWithOther

{- | Spawn an enemy and defeat it, carrying the traits the real Defeated message
would carry.
-}
defeatEnemy :: CardDef -> [Trait] -> TestAppT ()
defeatEnemy def traits = do
  location <- testLocation
  enemy <- testEnemyWithDef def id
  enemy `spawnAt` location
  run $ Defeated (toTarget enemy) (toCardId enemy) (TestSource mempty) traits

-- | testAssetWithDef leaves an asset Unplaced with no controller.
controlledBy :: Investigator -> Asset.AssetAttrs -> Asset.AssetAttrs
controlledBy i = (Asset.controllerL ?~ toId i) . (Asset.placementL .~ InPlayArea (toId i))

spec :: Spec
spec = describe "The Innsmouth Conspiracy achievements" $ do
  context "Would You Just Die Already" $ do
    -- The Amalgam is never discarded — its forced ability returns it to The
    -- Depths with its damage cleared — so each kill produces another Defeated.
    let defeatTheAmalgam = defeatEnemy Enemies.theAmalgam [DeepOne]

    it "is earned on the fifth defeat of The Amalgam" . gameTest $ \_ -> do
      asTheInnsmouthConspiracyScenario "07041"
      earned <- didEarnInnsmouth WouldYouJustDieAlready
      replicateM_ 4 defeatTheAmalgam
      earned `refShouldBe` False
      defeatTheAmalgam
      earned `refShouldBe` True

    it "is not earned after four" . gameTest $ \_ -> do
      asTheInnsmouthConspiracyScenario "07041"
      earned <- didEarnInnsmouth WouldYouJustDieAlready
      replicateM_ 4 defeatTheAmalgam
      earned `refShouldBe` False

    it "does not count other enemies" . gameTest $ \_ -> do
      asTheInnsmouthConspiracyScenario "07041"
      earned <- didEarnInnsmouth WouldYouJustDieAlready
      replicateM_ 5 $ defeatEnemy Enemies.lurkingDeepOne [DeepOne]
      earned `refShouldBe` False

    it "is not earned while achievements are disabled" . gameTest $ \_ -> do
      asTheInnsmouthConspiracyScenario "07041"
      overTest \g -> g {gameSettings = (gameSettings g) {settingsAchievementsEnabled = False}}
      earned <- didEarnInnsmouth WouldYouJustDieAlready
      replicateM_ 5 defeatTheAmalgam
      earned `refShouldBe` False

  context "Elementary, Dear Dawson" $ do
    -- The Search for Agent Harper defers a DoStep 1 for each correct guess as it
    -- advances: one for the suspect, one for the hideout.
    let guessCorrectly = run $ DoStep 1 $ advanceActMsg Acts.theSearchForAgentHarper

    it "is earned after guessing both the suspect and the hideout" . gameTest $ \_ -> do
      asTheInnsmouthConspiracyScenario "07056"
      earned <- didEarnInnsmouth ElementaryDearDawson
      guessCorrectly
      earned `refShouldBe` False
      guessCorrectly
      earned `refShouldBe` True

    it "is not earned after only one correct guess" . gameTest $ \_ -> do
      asTheInnsmouthConspiracyScenario "07056"
      earned <- didEarnInnsmouth ElementaryDearDawson
      guessCorrectly
      earned `refShouldBe` False

  context "Ain't Nothin Gonna Break My Stride" $ do
    {- In Too Deep keeps its barrier counts in the scenario meta and updates them
    when it processes the decrement — after the campaign has seen it — so the
    detection applies the decrement to its own copy. The harness shell does not
    run In Too Deep's own handler, so the meta is seeded directly.
    -}
    let seedBarriers edges = run $ SetScenarioMeta $ toJSON $ InTooDeep.Meta $ Map.fromList edges

    it "is earned when the last barrier comes down" . gameTest $ \_ -> do
      asTheInnsmouthConspiracyScenario "07123"
      a <- testLocation
      b <- testLocation
      c <- testLocation
      seedBarriers [(sortedPair (toId a) (toId b), 0), (sortedPair (toId b) (toId c), 1)]
      earned <- didEarnInnsmouth AintNothinGonnaBreakMyStride
      run $ ScenarioCountDecrementBy (Barriers (toId b) (toId c)) 1
      earned `refShouldBe` True

    it "is not earned while a barrier remains elsewhere" . gameTest $ \_ -> do
      asTheInnsmouthConspiracyScenario "07123"
      a <- testLocation
      b <- testLocation
      c <- testLocation
      seedBarriers [(sortedPair (toId a) (toId b), 1), (sortedPair (toId b) (toId c), 1)]
      earned <- didEarnInnsmouth AintNothinGonnaBreakMyStride
      run $ ScenarioCountDecrementBy (Barriers (toId b) (toId c)) 1
      earned `refShouldBe` False

    it "is not earned by thinning a stack of barriers on the same edge" . gameTest $ \_ -> do
      asTheInnsmouthConspiracyScenario "07123"
      a <- testLocation
      b <- testLocation
      seedBarriers [(sortedPair (toId a) (toId b), 3)]
      earned <- didEarnInnsmouth AintNothinGonnaBreakMyStride
      run $ ScenarioCountDecrementBy (Barriers (toId a) (toId b)) 1
      earned `refShouldBe` False

  context "Speeding Ticket" $ do
    -- Falcon Point Approach's objective is the only thing that advances Pedal to
    -- the Metal, so its advance IS "reached Falcon Point Approach".
    let reachFalconPoint = run $ advanceActMsg Acts.pedalToTheMetal
        aCar self = testAssetWithDef Assets.thomasDawsonsCarRunning (controlledBy self) self

    it "is earned reaching Falcon Point Approach with a clean run" . gameTest $ \_ -> do
      asTheInnsmouthConspiracyScenario "07198"
      earned <- didEarnInnsmouth SpeedingTicket
      reachFalconPoint
      earned `refShouldBe` True

    it "is not earned after voluntarily stopping a car" . gameTest $ \self -> do
      asTheInnsmouthConspiracyScenario "07198"
      car <- aCar self
      earned <- didEarnInnsmouth SpeedingTicket
      run $ ReplaceAsset (toId car) Assets.thomasDawsonsCarStopped
      reachFalconPoint
      earned `refShouldBe` False

    it "is not earned after getting out of a car" . gameTest $ \self -> do
      asTheInnsmouthConspiracyScenario "07198"
      car <- aCar self
      road <- testLocation
      run $ PlaceInvestigator (toId self) (InVehicle $ toId car)
      earned <- didEarnInnsmouth SpeedingTicket
      run $ PlaceInvestigator (toId self) (AtLocation $ toId road)
      reachFalconPoint
      earned `refShouldBe` False

    it "is not earned after driving into a Long Way Around" . gameTest $ \self -> do
      asTheInnsmouthConspiracyScenario "07198"
      car <- aCar self
      longWay <- testLocationWithDef Locations.longWayAround id
      earned <- didEarnInnsmouth SpeedingTicket
      run $ PlaceAsset (toId car) (AtLocation $ toId longWay)
      reachFalconPoint
      earned `refShouldBe` False

    it "still allows driving onto an ordinary road" . gameTest $ \self -> do
      asTheInnsmouthConspiracyScenario "07198"
      car <- aCar self
      road <- testLocation
      earned <- didEarnInnsmouth SpeedingTicket
      run $ PlaceAsset (toId car) (AtLocation $ toId road)
      reachFalconPoint
      earned `refShouldBe` True

  context "You're Locked In Here With Me" $ do
    -- A Light in the Fog flags a captured investigator with its own
    -- per-investigator scenario message.
    let capture i = run $ ForInvestigator (toId i) (ScenarioSpecific "captured" Null)

    it "is earned completing A Light in the Fog with nobody captured" . gameTest $ \_ -> do
      asTheInnsmouthConspiracyScenario "07231"
      earned <- didEarnInnsmouth YoureLockedInHereWithMe
      run $ EndOfGame Nothing
      earned `refShouldBe` True

    it "is not earned once an investigator has been captured" . gameTest $ \self -> do
      asTheInnsmouthConspiracyScenario "07231"
      earned <- didEarnInnsmouth YoureLockedInHereWithMe
      capture self
      run $ EndOfGame Nothing
      earned `refShouldBe` False

    -- Being freed later does not un-capture them.
    it "is not earned when a captured investigator is freed again" . gameTest $ \self -> do
      asTheInnsmouthConspiracyScenario "07231"
      earned <- didEarnInnsmouth YoureLockedInHereWithMe
      capture self
      run $ ForInvestigator (toId self) (ScenarioSpecific "free" Null)
      run $ EndOfGame Nothing
      earned `refShouldBe` False

  context "Fish Out of Water" $ do
    it "is earned when every investigator still has a Diving Suit" . gameTest $ \self -> do
      asTheInnsmouthConspiracyScenario "07311"
      _ <- testAssetWithDef Assets.divingSuit (controlledBy self) self
      earned <- didEarnInnsmouth FishOutOfWater
      run $ EndOfGame Nothing
      earned `refShouldBe` True

    it "is not earned when an investigator has lost theirs" . gameTest $ \_ -> do
      asTheInnsmouthConspiracyScenario "07311"
      earned <- didEarnInnsmouth FishOutOfWater
      run $ EndOfGame Nothing
      earned `refShouldBe` False

    it "is not earned when only some investigators are suited" . gameTest $ \self -> do
      asTheInnsmouthConspiracyScenario "07311"
      _ <- addInvestigator Investigators.dianaStanley
      _ <- testAssetWithDef Assets.divingSuit (controlledBy self) self
      earned <- didEarnInnsmouth FishOutOfWater
      run $ EndOfGame Nothing
      earned `refShouldBe` False

  context "Don't Wake Daddy" $ do
    -- Finishing The Lair of Dagon with Dagon asleep latches half the achievement;
    -- the earn happens when Into the Maelstrom is finished with both asleep.
    let finishTheLair = atScenario "07274" >> run (EndOfGame Nothing)
        finishTheMaelstrom = atScenario "07311" >> run (EndOfGame Nothing)
        wakeHydra = do
          location <- testLocation
          hydra <- testEnemyWithDef Enemies.hydraDeepInSlumber id
          hydra `spawnAt` location
          awakened <- genCard Enemies.hydraAwakenedAndEnraged
          run $ ReplaceEnemy (toId hydra) awakened Swap

    it "is earned finishing both scenarios with both gods asleep" . gameTest $ \_ -> do
      asTheInnsmouthConspiracy
      earned <- didEarnInnsmouth DontWakeDaddy
      finishTheLair
      earned `refShouldBe` False
      finishTheMaelstrom
      earned `refShouldBe` True

    it "is not earned once Dagon has awakened" . gameTest $ \_ -> do
      asTheInnsmouthConspiracy
      earned <- didEarnInnsmouth DontWakeDaddy
      run $ Record (toCampaignLogKey DagonHasAwakened)
      finishTheLair
      finishTheMaelstrom
      earned `refShouldBe` False

    it "is not earned once Hydra has awakened" . gameTest $ \_ -> do
      asTheInnsmouthConspiracy
      earned <- didEarnInnsmouth DontWakeDaddy
      finishTheLair
      atScenario "07311"
      wakeHydra
      run $ EndOfGame Nothing
      earned `refShouldBe` False

    it "is not earned without having finished The Lair of Dagon" . gameTest $ \_ -> do
      asTheInnsmouthConspiracy
      earned <- didEarnInnsmouth DontWakeDaddy
      finishTheMaelstrom
      earned `refShouldBe` False

  context "Gone Fishing" $ do
    let defeatADeepOne = defeatEnemy Enemies.lurkingDeepOne [DeepOne]

    it "is earned on the twentieth Deep One defeated" . gameTest $ \_ -> do
      asTheInnsmouthConspiracy
      earned <- didEarnInnsmouth GoneFishing
      replicateM_ 19 defeatADeepOne
      earned `refShouldBe` False
      defeatADeepOne
      earned `refShouldBe` True

    it "is not earned after nineteen" . gameTest $ \_ -> do
      asTheInnsmouthConspiracy
      earned <- didEarnInnsmouth GoneFishing
      replicateM_ 19 defeatADeepOne
      earned `refShouldBe` False

    it "does not count enemies without the Deep One trait" . gameTest $ \_ -> do
      asTheInnsmouthConspiracy
      earned <- didEarnInnsmouth GoneFishing
      replicateM_ 20 $ defeatEnemy Enemies.huntingNightgaunt []
      earned `refShouldBe` False

  context "Bigger Fish to Fry" $ do
    it "is earned finishing the campaign with no Deep One defeated" . gameTest $ \_ -> do
      asTheInnsmouthConspiracy
      earned <- didEarnInnsmouth BiggerFishToFry
      finishTheCampaign
      earned `refShouldBe` True

    it "is not earned after a single Deep One is defeated" . gameTest $ \_ -> do
      asTheInnsmouthConspiracy
      earned <- didEarnInnsmouth BiggerFishToFry
      defeatEnemy Enemies.lurkingDeepOne [DeepOne]
      finishTheCampaign
      earned `refShouldBe` False

  context "Full Build" $ do
    let equip self def = void $ testAssetWithDef def (controlledBy self) self
        play self def = run . CardEnteredPlay (toId self) =<< genCard def

    it "is earned when one investigator holds all three relics" . gameTest $ \self -> do
      asTheInnsmouthConspiracy
      equip self Assets.wavewornIdol
      equip self Assets.awakenedMantle
      earned <- didEarnInnsmouth FullBuild
      play self Assets.headdressOfYhaNthlei
      earned `refShouldBe` True

    it "is earned when the third relic is handed over" . gameTest $ \self -> do
      asTheInnsmouthConspiracy
      equip self Assets.wavewornIdol
      equip self Assets.awakenedMantle
      headdress <- testAssetWithDef Assets.headdressOfYhaNthlei id self
      earned <- didEarnInnsmouth FullBuild
      run $ TakeControlOfAsset (toId self) (toId headdress)
      earned `refShouldBe` True

    it "is not earned with only two relics" . gameTest $ \self -> do
      asTheInnsmouthConspiracy
      equip self Assets.wavewornIdol
      earned <- didEarnInnsmouth FullBuild
      play self Assets.awakenedMantle
      earned `refShouldBe` False

    it "is not earned when the relics are spread across investigators" . gameTest $ \self -> do
      asTheInnsmouthConspiracy
      other <- addInvestigator Investigators.dianaStanley
      equip self Assets.wavewornIdol
      equip other Assets.awakenedMantle
      earned <- didEarnInnsmouth FullBuild
      play self Assets.headdressOfYhaNthlei
      earned `refShouldBe` False

  context "\"You Wake Up In A Room...\"" $ do
    let recoverMemories memories =
          run $ RecordSetInsert (toCampaignLogKey MemoriesRecovered) (map recorded memories)
        allMemories =
          [ AMeetingWithThomasDawson
          , ABattleWithAHorrifyingDevil
          , ADecisionToStickTogether
          , AnEncounterWithASecretCult
          , ADealWithJoeSargent
          , AFollowedLead
          , AnIntervention
          , AJailbreak
          , DiscoveryOfAStrangeIdol
          , DiscoveryOfAnUnholyMantle
          , DiscoveryOfAMysticalRelic
          , AConversationWithMrMoore
          , TheLifecycleOfADeepOne
          , AStingingBetrayal
          ]
        allItems =
          [ "AMeetingWithThomasDawson"
          , "ABattleWithAHorrifyingDevil"
          , "ADecisionToStickTogether"
          , "AnEncounterWithASecretCult"
          , "ADealWithJoeSargent"
          , "AFollowedLead"
          , "AnIntervention"
          , "AJailbreak"
          , "DiscoveryOfAStrangeIdol"
          , "DiscoveryOfAnUnholyMantle"
          , "DiscoveryOfAMysticalRelic"
          , "AConversationWithMrMoore"
          , "TheLifecycleOfADeepOne"
          , "AStingingBetrayal"
          , "TheHorribleTruth"
          ]

    it "reports only the memories actually recovered" . gameTest $ \_ -> do
      asTheInnsmouthConspiracy
      recoverMemories [AMeetingWithThomasDawson, AJailbreak]
      progressed <-
        didProgressInnsmouth YouWakeUpInARoom ["AMeetingWithThomasDawson", "AJailbreak"]
      finishTheCampaign
      progressed `refShouldBe` True

    -- The Horrible Truth is recorded by the epilogue itself, i.e. after the
    -- detection dispatches, so it is derived from having all fourteen memories.
    it "reports The Horrible Truth once every memory is recovered" . gameTest $ \_ -> do
      asTheInnsmouthConspiracy
      recoverMemories allMemories
      progressed <- didProgressInnsmouth YouWakeUpInARoom allItems
      finishTheCampaign
      progressed `refShouldBe` True

    it "does not report The Horrible Truth one memory short" . gameTest $ \_ -> do
      asTheInnsmouthConspiracy
      recoverMemories (drop 1 allMemories)
      progressed <- didProgressInnsmouth YouWakeUpInARoom (drop 1 $ take 14 allItems)
      finishTheCampaign
      progressed `refShouldBe` True

    it "reports nothing when no memory was recovered" . gameTest $ \_ -> do
      asTheInnsmouthConspiracy
      progressed <- didProgressInnsmouth YouWakeUpInARoom []
      finishTheCampaign
      progressed `refShouldBe` False

  context "winning the campaign" $ do
    it "earns Line in the Sand with three active ultimatums" . gameTest $ \_ -> do
      asTheInnsmouthConspiracy
      withUltimatums [UltimatumOfFailure, UltimatumOfHardship, UltimatumOfDread]
      earned <- didEarnInnsmouth InnsmouthLineInTheSand
      finishTheCampaign
      earned `refShouldBe` True

    it "does not earn Line in the Sand with only two" . gameTest $ \_ -> do
      asTheInnsmouthConspiracy
      withUltimatums [UltimatumOfFailure, UltimatumOfHardship]
      earned <- didEarnInnsmouth InnsmouthLineInTheSand
      finishTheCampaign
      earned `refShouldBe` False

    it "earns Innsmouth Expertise on Expert" . gameTest $ \_ -> do
      asTheInnsmouthConspiracyWith Expert
      earned <- didEarnInnsmouth InnsmouthExpertise
      finishTheCampaign
      earned `refShouldBe` True

    it "does not earn Innsmouth Expertise below Expert" . gameTest $ \_ -> do
      asTheInnsmouthConspiracyWith Hard
      earned <- didEarnInnsmouth InnsmouthExpertise
      finishTheCampaign
      earned `refShouldBe` False

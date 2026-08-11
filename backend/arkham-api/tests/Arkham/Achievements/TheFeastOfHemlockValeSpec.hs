{- | The Feast of Hemlock Vale achievement detection.

The detections live on the campaign entity, so the specs drive them with the
same messages the campaign emits (relationship record counts, the record Fate of
the Vale's Resolution 2 writes, the epilogue step) rather than replaying whole
scenarios. Reaching 'CampaignStep EpilogueStep' is how the campaign is
completed: every Fate of the Vale ending but the no-resolution one routes there.
-}
module Arkham.Achievements.TheFeastOfHemlockValeSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Types qualified as Campaign
import Arkham.CampaignLogKey (toCampaignLogKey)
import Arkham.CampaignStep (CampaignStep (EpilogueStep, ScenarioStep), continue)
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (Resident (..), relationshipKey)
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers qualified as Hemlock
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.DamageEffect (nonAttack)
import Arkham.Difficulty
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Investigator.Cards qualified as Investigators
import Helpers.Achievements
import Helpers.UltimatumsAndBoons (Ultimatum (..), withUltimatums)
import TestImport.New

-- | Every surviving ending of Fate of the Vale routes through the epilogue.
finishTheCampaign :: TestAppT ()
finishTheCampaign = run $ CampaignStep EpilogueStep

-- | Defeat an enemy of the given def, the way the campaign sees it.
defeatEnemy :: CardDef -> TestAppT ()
defeatEnemy def = do
  enemy <- testEnemyWithDef def id
  run $ Defeated (toTarget enemy) (toCardId enemy) (TestSource mempty) []

{- | Seed the day/time meta the campaign builds at its prologue. A spec that runs a
scenario's own end-of-game handling needs it, since those read the meta back.
-}
withHemlockMeta :: TestAppT ()
withHemlockMeta = withHemlockMetaOn Hemlock.Day1

-- | The same, on a chosen day.
withHemlockMetaOn :: Hemlock.Day -> TestAppT ()
withHemlockMetaOn day = withHemlockMetaAt day Hemlock.Day

{- | The same, at a chosen point of a chosen day: the evening skips are sensitive to
both, since each dawn also picks the coming scenario.
-}
withHemlockMetaAt :: Hemlock.Day -> Hemlock.Time -> TestAppT ()
withHemlockMetaAt day time = do
  overTest \g ->
    g
      { gameMode =
          first
            (overAttrs (Campaign.metaL .~ toJSON Hemlock.initMeta {Hemlock.day = day, Hemlock.time = time}))
            (gameMode g)
      }
  tick

-- | Set a resident's Relationship Level, which the campaign log keeps as a count.
relationship :: Resident -> Int -> TestAppT ()
relationship resident n = run $ RecordCount (relationshipKey resident) n

spec :: Spec
spec = describe "The Feast of Hemlock Vale achievements" $ do
  context "Aperitif" $ do
    it "is earned completing the campaign on any difficulty" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      earned <- didEarnHemlockVale Aperitif
      finishTheCampaign
      earned `refShouldBe` True

    -- Becoming the true feast calls gameOver without ever reaching the epilogue.
    it "is not earned without reaching the epilogue" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      earned <- didEarnHemlockVale Aperitif
      run $ Record $ toCampaignLogKey TheInvestigatorsBecameTheTrueFeastOfHemlockVale
      earned `refShouldBe` False

  context "High Dive" $ do
    it "is earned sacrificing yourselves for the Vale" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      earned <- didEarnHemlockVale HighDive
      run $ Record $ toCampaignLogKey TheInvestigatorsSacrificedThemselvesForTheVale
      earned `refShouldBe` True

    -- Resolution 1 is Marquez giving herself instead, which is not your sacrifice.
    {- The R2 record is BOTH High Dive and one of Unshattered's endings. A separate
    case alternative for High Dive would swallow it (case alternatives are
    first-match) and Unshattered would silently miss that ending.
    -}
    it "also reports the ending for Unshattered" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      earned <- didEarnHemlockVale HighDive
      progressed <-
        didProgressHemlockVale Unshattered ["TheInvestigatorsSacrificedThemselves"]
      run $ Record $ toCampaignLogKey TheInvestigatorsSacrificedThemselvesForTheVale
      earned `refShouldBe` True
      progressed `refShouldBe` True

    it "is not earned when Marquez sacrifices herself" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      earned <- didEarnHemlockVale HighDive
      run $ Record $ toCampaignLogKey DrMarquezSacrificedHerselfForTheVale
      earned `refShouldBe` False

  {- The relationship achievements. Each is "at least" the printed level, so the
  boundary is checked on both sides.
  -}
  context "relationship levels" $ do
    for_
      [ ("Know Your Place", MotherRachel, 3, KnowYourPlace)
      , ("Heart of Steel", JudithPark, 7, HeartOfSteel)
      , ("Holding Out for a Himbo", TheoPeters, 7, HoldingOutForAHimbo)
      ]
      \(title, resident, level, achievement) -> do
        it ("earns " <> title <> " at the printed level") . gameTest $ \_ -> do
          asTheFeastOfHemlockVale
          earned <- didEarnHemlockVale achievement
          relationship resident level
          finishTheCampaign
          earned `refShouldBe` True

        it ("does not earn " <> title <> " one level short") . gameTest $ \_ -> do
          asTheFeastOfHemlockVale
          earned <- didEarnHemlockVale achievement
          relationship resident (level - 1)
          finishTheCampaign
          earned `refShouldBe` False

    -- The three are independent: one resident's level does not stand in for another's.
    it "does not cross residents over" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      earned <- didEarnHemlockVale HeartOfSteel
      relationship TheoPeters 7
      finishTheCampaign
      earned `refShouldBe` False

  context "Captivating Scream" $ do
    it "is earned winning as Patrice Hathaway" . gameTest $ \_ -> do
      void $ addInvestigator Investigators.patriceHathaway
      asTheFeastOfHemlockVale
      earned <- didEarnHemlockVale CaptivatingScream
      finishTheCampaign
      earned `refShouldBe` True

    {- Resolution 2 kills everyone on the way to the epilogue, so the matcher has
    to include eliminated investigators.
    -}
    it "is earned even when Patrice sacrificed herself" . gameTest $ \_ -> do
      patrice <- addInvestigator Investigators.patriceHathaway
      asTheFeastOfHemlockVale
      earned <- didEarnHemlockVale CaptivatingScream
      run $ InvestigatorKilled (TestSource mempty) (toId patrice)
      finishTheCampaign
      earned `refShouldBe` True

    it "is not earned as a different investigator" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      earned <- didEarnHemlockVale CaptivatingScream
      finishTheCampaign
      earned `refShouldBe` False

  {- "Unshattered" reports each ending as its record is written; the API layer
  accumulates the six across playthroughs.
  -}
  context "Unshattered" $ do
    it "reports an ending as it is achieved" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      progressed <- didProgressHemlockVale Unshattered ["TheValeWasSaved"]
      run $ Record $ toCampaignLogKey TheValeWasSaved
      progressed `refShouldBe` True

    it "accumulates the endings seen so far" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      progressed <- didProgressHemlockVale Unshattered ["TheValeBurned", "TheValeWasSaved"]
      run $ Record $ toCampaignLogKey TheValeWasSaved
      run $ Record $ toCampaignLogKey TheValeBurned
      progressed `refShouldBe` True

    -- The losing ending counts too: it is one of the campaign's endings.
    it "reports becoming the true feast" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      progressed <- didProgressHemlockVale Unshattered ["BecameTheTrueFeast"]
      run $ Record $ toCampaignLogKey TheInvestigatorsBecameTheTrueFeastOfHemlockVale
      progressed `refShouldBe` True

  context "Let's Do the Time Warp!" $ do
    it "is earned replaying the evening" . gameTest $ \_ -> do
      asTheFeastOfHemlockValeScenario "10679b"
      earned <- didEarnHemlockVale LetsDoTheTimeWarp
      run $ CrossOutRecord $ toCampaignLogKey TheInvestigatorsBelieved
      earned `refShouldBe` True

    it "is not earned in a different scenario" . gameTest $ \_ -> do
      asTheFeastOfHemlockValeScenario "10651"
      earned <- didEarnHemlockVale LetsDoTheTimeWarp
      run $ CrossOutRecord $ toCampaignLogKey TheInvestigatorsBelieved
      earned `refShouldBe` False

  context "Oblivion Shmoblivion" $ do
    let emissaries =
          [ Enemies.cosmicEmissaryTheAbyss
          , Enemies.cosmicEmissaryTheMiasma
          , Enemies.cosmicEmissaryTheBrilliance
          , Enemies.cosmicEmissaryThePhantasm
          ]
        toVictory def = do
          enemy <- testEnemyWithDef def id
          run $ AddToVictory Nothing (toTarget enemy)

    it "is earned with every Cosmic Emissary in the victory display" . gameTest $ \_ -> do
      asTheFeastOfHemlockValeScenario "10651"
      earned <- didEarnHemlockVale OblivionShmoblivion
      traverse_ toVictory emissaries
      run $ EndOfGame Nothing
      earned `refShouldBe` True

    it "is not earned with one still at large" . gameTest $ \_ -> do
      asTheFeastOfHemlockValeScenario "10651"
      earned <- didEarnHemlockVale OblivionShmoblivion
      traverse_ toVictory (drop 1 emissaries)
      run $ EndOfGame Nothing
      earned `refShouldBe` False

  context "Life of the Party" $ do
    let allResidents n = for_ [minBound .. maxBound] (`relationship` n)

    it "is earned with every resident at level 2" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      earned <- didEarnHemlockVale LifeOfTheParty
      allResidents 2
      finishTheCampaign
      earned `refShouldBe` True

    -- All eight residents count, not just the five Best Friends names.
    it "is not earned with one resident left behind" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      earned <- didEarnHemlockVale LifeOfTheParty
      allResidents 2
      relationship GideonMizrah 1
      finishTheCampaign
      earned `refShouldBe` False

  {- Each evening resolves by pushing NextCampaignStep: the optional scenario if
  you search, a survey scenario if you skip it. 'skipTo' is the skip.
  -}
  context "Colour Outside the Lines" $ do
    let skipTo step = run $ NextCampaignStep (continue step)
        onSecondEvening = withHemlockMetaAt Hemlock.Day2 Hemlock.Night

    it "is earned when the second evening skips The Longest Night" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      onSecondEvening
      earned <- didEarnHemlockVale ColourOutsideTheLines
      skipTo (ScenarioStep "10501")
      earned `refShouldBe` True

    -- Choosing to follow Dr. Marquez IS The Longest Night, so it is not skipped.
    it "is not earned when the second evening picks The Longest Night" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      onSecondEvening
      earned <- didEarnHemlockVale ColourOutsideTheLines
      skipTo (ScenarioStep "10626")
      earned `refShouldBe` False

    {- The dawn of the second day picks the day's survey scenario, which is another
    explicit NextCampaignStep on Day 2 — but the evening has not been offered yet.
    -}
    it "is not earned when the second day's daytime survey is chosen" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      withHemlockMetaAt Hemlock.Day2 Hemlock.Day
      earned <- didEarnHemlockVale ColourOutsideTheLines
      skipTo (ScenarioStep "10501")
      earned `refShouldBe` False

    -- The first evening's skip is not enough on its own; the second is still open.
    it "is not earned on the first evening's skip alone" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      withHemlockMetaAt Hemlock.Day1 Hemlock.Night
      earned <- didEarnHemlockVale ColourOutsideTheLines
      skipTo (ScenarioStep "10501")
      earned `refShouldBe` False

    for_ [("The Twisted Hollow", "10605"), ("The Longest Night", "10626")] \(title, code) ->
      it ("is not earned after playing " <> title) . gameTest $ \_ -> do
        asTheFeastOfHemlockValeScenario code
        withHemlockMeta
        run Setup
        earned <- didEarnHemlockVale ColourOutsideTheLines
        finishTheCampaign
        earned `refShouldBe` False

    it "is earned at the epilogue having skipped both" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      earned <- didEarnHemlockVale ColourOutsideTheLines
      finishTheCampaign
      earned `refShouldBe` True

  context "Dancing Queen" $ do
    let dance = run . Record . toCampaignLogKey
        partners =
          [ toCampaignLogKey (LeahAtwoodNotes LeahSharedADance)
          , toCampaignLogKey (SimeonAtwoodNotes SimeonSharedADance)
          , toCampaignLogKey (TheoPetersNotes TheoSharedADance)
          , toCampaignLogKey (JudithParkNotes JudithSharedADance)
          ]

    it "is earned dancing with a fourth resident" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      earned <- didEarnHemlockVale DancingQueen
      traverse_ (run . Record) partners
      earned `refShouldBe` True

    it "is not earned after only three" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      earned <- didEarnHemlockVale DancingQueen
      traverse_ (run . Record) (take 3 partners)
      earned `refShouldBe` False

    -- Four dances with the same partner is one partner, not four.
    it "is not earned dancing with one resident repeatedly" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      earned <- didEarnHemlockVale DancingQueen
      traverse_ (const $ dance (LeahAtwoodNotes LeahSharedADance)) [1 :: Int .. 4]
      earned `refShouldBe` False

  context "Settling the Score" $ do
    it "is earned defeating the Thing in the Depths" . gameTest $ \_ -> do
      asTheFeastOfHemlockValeScenario "10588"
      earned <- didEarnHemlockVale SettlingTheScore
      defeatEnemy Enemies.thingInTheDepths
      earned `refShouldBe` True

    it "is not earned in a different scenario" . gameTest $ \_ -> do
      asTheFeastOfHemlockValeScenario "10569"
      earned <- didEarnHemlockVale SettlingTheScore
      defeatEnemy Enemies.thingInTheDepths
      earned `refShouldBe` False

  context "\"Here, Crabby Crabby!\"" $ do
    let flipHybrid = do
          enemy <- testEnemyWithDef Enemies.limulusHybridInTheLight id
          run $ Flip (InvestigatorId "01001") (TestSource mempty) (toTarget enemy)

    it "is earned on the eighth flip" . gameTest $ \_ -> do
      asTheFeastOfHemlockValeScenario "10569"
      earned <- didEarnHemlockVale HereCrabbyCrabby
      traverse_ (const flipHybrid) [1 :: Int .. 8]
      earned `refShouldBe` True

    it "is not earned after seven" . gameTest $ \_ -> do
      asTheFeastOfHemlockValeScenario "10569"
      earned <- didEarnHemlockVale HereCrabbyCrabby
      traverse_ (const flipHybrid) [1 :: Int .. 7]
      earned `refShouldBe` False

  context "A Different Kind of Sting Ops" $ do
    it "is earned finishing without the Brood Queen" . gameTest $ \_ -> do
      asTheFeastOfHemlockValeScenario "10549"
      withHemlockMeta
      earned <- didEarnHemlockVale ADifferentKindOfStingOps
      run $ EndOfGame Nothing
      earned `refShouldBe` True

    it "is not earned once she spawns" . gameTest $ \_ -> do
      asTheFeastOfHemlockValeScenario "10549"
      withHemlockMeta
      earned <- didEarnHemlockVale ADifferentKindOfStingOps
      location <- testLocation
      card <- genCard Enemies.broodQueenDyingMother
      (_, createMsg) <- createEnemyAt card (toId location) Nothing
      run createMsg
      run $ EndOfGame Nothing
      earned `refShouldBe` False

  context "Bear Necessities" $ do
    let bear = testEnemyWithDef Enemies.ursineHybridStarvingAbomination id

    it "is earned defeating the bear with scenario effects only" . gameTest $ \_ -> do
      asTheFeastOfHemlockValeScenario "10626"
      ursine <- bear
      earned <- didEarnHemlockVale BearNecessities
      run $ Damaged (toTarget ursine) (nonAttack Nothing ScenarioSource 2)
      run $ Defeated (toTarget ursine) (toCardId ursine) ScenarioSource []
      earned `refShouldBe` True

    {- Anything that is not a scenario card effect disqualifies it. A plain fight
    action is sourced to the INVESTIGATOR rather than to a weapon, so that case is
    driven explicitly — checking only for assets and events let it through.
    -}
    it "is not earned after damaging it with a weapon" . gameTest $ \self -> do
      asTheFeastOfHemlockValeScenario "10626"
      ursine <- bear
      weapon <- testAssetWithDef Assets.knife id self
      earned <- didEarnHemlockVale BearNecessities
      run $ Damaged (toTarget ursine) (nonAttack Nothing (toSource weapon) 2)
      run $ Defeated (toTarget ursine) (toCardId ursine) ScenarioSource []
      earned `refShouldBe` False

    {- The REAL fight action, not a synthesised Damaged. A basic attack is sourced
    to the ENEMY's own fight ability (UseAbilitySource iid (EnemySource _) 100), so
    unwrapping it naively makes an ordinary attack look like a scenario effect.
    -}
    it "is not earned after fighting it" . gameTest $ \self -> do
      asTheFeastOfHemlockValeScenario "10626"
      ursine <- bear & prop @"health" 3 & prop @"fight" 0
      location <- testLocation
      self `moveTo` location
      ursine `spawnAt` location
      earned <- didEarnHemlockVale BearNecessities
      setChaosTokens [Zero]
      void $ fightEnemy self ursine
      startSkillTest
      applyResults
      ursine.damage `shouldReturn` 1
      run $ Defeated (toTarget ursine) (toCardId ursine) ScenarioSource []
      earned `refShouldBe` False

  {- "Best Friends Forever!" reports the moment a resident reaches level 6, so it
  shows progress mid-campaign rather than only at the end.
  -}
  context "Best Friends Forever!" $ do
    it "reports a resident on reaching level 6" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      progressed <- didProgressHemlockVale BestFriendsForever ["WilliamHemlock"]
      relationship WilliamHemlock 6
      progressed `refShouldBe` True

    -- The real flow raises a level one step at a time.
    it "reports when an increment reaches level 6" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      relationship WilliamHemlock 5
      progressed <- didProgressHemlockVale BestFriendsForever ["WilliamHemlock"]
      run $ IncrementRecordCount (relationshipKey WilliamHemlock) 1
      progressed `refShouldBe` True

    it "accumulates the residents reached so far" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      progressed <- didProgressHemlockVale BestFriendsForever ["LeahAtwood", "WilliamHemlock"]
      relationship WilliamHemlock 6
      relationship LeahAtwood 6
      progressed `refShouldBe` True

    it "does not report below level 6" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      progressed <- didProgressHemlockVale BestFriendsForever ["WilliamHemlock"]
      relationship WilliamHemlock 5
      progressed `refShouldBe` False

    -- Only the five named residents have a checklist box.
    it "does not report a resident outside the checklist" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      progressed <- didProgressHemlockVale BestFriendsForever ["JudithPark"]
      relationship JudithPark 6
      progressed `refShouldBe` False

  {- "A Strong, Silent Type": every codex trigger routes through the same
  scenario-specific message, so these drive it directly with the entry numbers the
  cards use.
  -}
  context "A Strong, Silent Type" $ do
    let triggerCodex self entry =
          run $ ScenarioSpecific "codex" (toJSON (toId self, ScenarioSource, entry :: Int))
        theta = 100 :: Int
        omega = 101 :: Int
        psi = 102 :: Int
        phi = 103 :: Int
        sigma = 104 :: Int

    it "is earned finishing without opening the codex" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      earned <- didEarnHemlockVale AStrongSilentType
      finishTheCampaign
      earned `refShouldBe` True

    -- A resident's parley is the player choosing to open the codex.
    it "is not earned after a voluntary entry" . gameTest $ \self -> do
      asTheFeastOfHemlockValeScenario "10501"
      earned <- didEarnHemlockVale AStrongSilentType
      triggerCodex self theta
      finishTheCampaign
      earned `refShouldBe` False

    {- Sigma and entry 17 are pushed by acts and agendas, so they are forced on the
    investigators wherever they appear.
    -}
    for_ [("Sigma", sigma), ("entry 17", 17)] \(title, entry) ->
      it ("is not disqualified by " <> title) . gameTest $ \self -> do
        asTheFeastOfHemlockValeScenario "10523"
        earned <- didEarnHemlockVale AStrongSilentType
        triggerCodex self entry
        finishTheCampaign
        earned `refShouldBe` True

    -- The Silent Heath's cave forces Omega, Psi and Phi on you.
    for_ [("Omega", omega), ("Psi", psi), ("Phi", phi)] \(title, entry) ->
      it ("is not disqualified by The Silent Heath's " <> title) . gameTest $ \self -> do
        asTheFeastOfHemlockValeScenario "10549"
        withHemlockMeta
        earned <- didEarnHemlockVale AStrongSilentType
        triggerCodex self entry
        finishTheCampaign
        earned `refShouldBe` True

    {- The same entries elsewhere ARE voluntary: Omega comes from Bertie's own
    ability outside the cave.
    -}
    it "is disqualified by Omega outside The Silent Heath" . gameTest $ \self -> do
      asTheFeastOfHemlockValeScenario "10626"
      withHemlockMeta
      earned <- didEarnHemlockVale AStrongSilentType
      triggerCodex self omega
      finishTheCampaign
      earned `refShouldBe` False

  context "winning the campaign" $ do
    it "earns Line in the Sand with three active ultimatums" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      withUltimatums [UltimatumOfFailure, UltimatumOfHardship, UltimatumOfDread]
      earned <- didEarnHemlockVale HemlockLineInTheSand
      finishTheCampaign
      earned `refShouldBe` True

    it "does not earn Line in the Sand with only two" . gameTest $ \_ -> do
      asTheFeastOfHemlockVale
      withUltimatums [UltimatumOfFailure, UltimatumOfHardship]
      earned <- didEarnHemlockVale HemlockLineInTheSand
      finishTheCampaign
      earned `refShouldBe` False

    it "earns Hemlock Expertise on Expert" . gameTest $ \_ -> do
      asTheFeastOfHemlockValeWith Expert
      earned <- didEarnHemlockVale HemlockExpertise
      finishTheCampaign
      earned `refShouldBe` True

    it "does not earn Hemlock Expertise below Expert" . gameTest $ \_ -> do
      asTheFeastOfHemlockValeWith Hard
      earned <- didEarnHemlockVale HemlockExpertise
      finishTheCampaign
      earned `refShouldBe` False

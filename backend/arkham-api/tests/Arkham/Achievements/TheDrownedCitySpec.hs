{- | The Drowned City achievement detection.

The detections live on the campaign entity, so most specs drive them with the
same messages the real scenarios and interludes emit (campaign log records,
enemy defeats, glyph record-set inserts, 'EndOfGame') rather than replaying
whole scenarios. Scenario-scoped detections use 'asTheDrownedCityScenario',
which swaps in a Gathering-shaped shell carrying the real scenario id — the
scenario's own behaviour never runs, which is exactly why the detections key on
messages rather than on resolutions.
-}
module Arkham.Achievements.TheDrownedCitySpec (spec) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types qualified as Asset
import Arkham.CampaignLogKey (recorded, toCampaignLogKey)
import Arkham.CampaignStep (CampaignStep (EpilogueStep, InterludeStep))
import Arkham.Campaigns.TheDrownedCity.Key
import Arkham.DamageEffect (nonAttack)
import Arkham.Difficulty
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Settings (settingsAchievementsEnabled)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.FloodLevel (FloodLevel (..))
import Arkham.Location.Grid (GridLocation (..), Pos (..))
import Arkham.Location.Types (revealedL)
import Arkham.Placement
import Arkham.Source
import Data.Text qualified as T
import Helpers.Achievements
import Helpers.UltimatumsAndBoons (Ultimatum (..), withUltimatums)
import TestImport.New

-- | Every surviving ending of the campaign routes through the epilogue.
finishTheCampaign :: TestAppT ()
finishTheCampaign = run $ CampaignStep EpilogueStep

{- | Face the Music's parley ability, the only legal killing blow for "This is
a Coup". An 'ActId' is the act's card code.
-}
faceTheMusicParley :: Source
faceTheMusicParley = AbilitySource (ActSource (ActId $ toCardCode Acts.faceTheMusic)) 1

{- | The message an act pushes as it advances (see 'advancedWithOther'). Scenario
completion is detected off this rather than off the end of the game, because
Court of the Ancients and The Drowned Quarter are beaten by resigning.
-}
advanceActMsg :: CardDef -> Message
advanceActMsg def = AdvanceAct (ActId $ toCardCode def) (TestSource mempty) AdvancedWithOther

spec :: Spec
spec = describe "The Drowned City achievements" $ do
  context "One First Last Job" $ do
    it "is earned when the campaign reaches its epilogue" . gameTest $ \_ -> do
      asTheDrownedCity
      earned <- didEarnDrownedCity OneFirstLastJob
      finishTheCampaign
      earned `refShouldBe` True

    it "is not earned by an ending record alone" . gameTest $ \_ -> do
      asTheDrownedCity
      earned <- didEarnDrownedCity OneFirstLastJob
      run $ Record (toCampaignLogKey CthulhuWasDrivenAway)
      earned `refShouldBe` False

    it "is not earned while achievements are disabled" . gameTest $ \_ -> do
      asTheDrownedCity
      overTest \g -> g {gameSettings = (gameSettings g) {settingsAchievementsEnabled = False}}
      earned <- didEarnDrownedCity OneFirstLastJob
      finishTheCampaign
      earned `refShouldBe` False

  context "Season Two" $ do
    {- Not detected, deliberately. It wants four investigators carried into The
    Drowned City via Epic Campaign Mode, each having completed a *different*
    previous campaign — a transfer that keeps trauma/story rewards, forfeits
    experience, and records "<name>'s total experience earned: X". The engine has
    no Epic Campaign Mode, so an investigator's provenance is not recorded and
    there is nothing to detect. This pins that: deriving it from which cycle the
    investigator cards came from was wrong, and awarded it to any hard/expert
    four-player game whose picks happened to span four cycles.
    -}
    it "is not earned merely by fielding investigators from four cycles" . gameTest $ \_ -> do
      asTheDrownedCityWith Hard
      _ <- addInvestigator Investigators.rolandBanks -- 01, Night of the Zealot
      _ <- addInvestigator Investigators.markHarrigan -- 03, The Path to Carcosa
      _ <- addInvestigator Investigators.carolynFern -- 05, The Circle Undone
      earned <- didEarnDrownedCity SeasonTwo
      finishTheCampaign
      earned `refShouldBe` False

  context "Cliff Diver" $ do
    it "is earned finishing the campaign with no diving suit brought along" . gameTest $ \_ -> do
      asTheDrownedCity
      earned <- didEarnDrownedCity CliffDiver
      finishTheCampaign
      earned `refShouldBe` True

    it "is not earned once a diving suit has entered play" . gameTest $ \self -> do
      asTheDrownedCity
      earned <- didEarnDrownedCity CliffDiver
      run . CardEnteredPlay (toId self) =<< genCard Assets.divingSuitTheDrownedCity
      finishTheCampaign
      earned `refShouldBe` False

  context "This is a Coup" $ do
    let defeatWith source def = do
          location <- testLocation
          boss <- testEnemyWithDef def id
          boss `spawnAt` location
          run $ Defeated (toTarget boss) (toCardId boss) source []

    it "is earned defeating both bosses with the act 3a parley" . gameTest $ \_ -> do
      asTheDrownedCityScenario "11501"
      earned <- didEarnDrownedCity ThisIsACoup
      defeatWith faceTheMusicParley Enemies.naomiOBannion
      earned `refShouldBe` False
      defeatWith faceTheMusicParley Enemies.sadieSheldon
      earned `refShouldBe` True

    it "is not earned when one boss is defeated by other means" . gameTest $ \_ -> do
      asTheDrownedCityScenario "11501"
      earned <- didEarnDrownedCity ThisIsACoup
      defeatWith faceTheMusicParley Enemies.naomiOBannion
      defeatWith (TestSource mempty) Enemies.sadieSheldon
      earned `refShouldBe` False

  context "Thorough Search" $ do
    it "is earned ending The Western Wall with every location revealed" . gameTest $ \_ -> do
      asTheDrownedCityScenario "11517"
      _ <- testLocationWith (revealedL .~ True)
      earned <- didEarnDrownedCity ThoroughSearch
      run $ EndOfGame Nothing
      earned `refShouldBe` True

    it "is not earned with an unrevealed location still in play" . gameTest $ \_ -> do
      asTheDrownedCityScenario "11517"
      _ <- testLocationWith (revealedL .~ True)
      _ <- testLocationWith (revealedL .~ False)
      earned <- didEarnDrownedCity ThoroughSearch
      run $ EndOfGame Nothing
      earned `refShouldBe` False

  context "Tidal Flip Minigame" $ do
    -- Completing the scenario is Reactivate the Core advancing, not the end of
    -- the game: these scenarios are beaten by resigning, so no investigator
    -- liveness check can stand in for "completed".
    let completeTheDrownedQuarter = run $ advanceActMsg Acts.reactivateTheCore

    it "is earned with every location revealed and dry" . gameTest $ \_ -> do
      asTheDrownedCityScenario "11536"
      _ <- testLocationWith (revealedL .~ True)
      earned <- didEarnDrownedCity TidalFlipMinigame
      completeTheDrownedQuarter
      earned `refShouldBe` True

    it "is not earned with a flooded location" . gameTest $ \_ -> do
      asTheDrownedCityScenario "11536"
      location <- testLocationWith (revealedL .~ True)
      run $ SetFloodLevel (toId location) PartiallyFlooded
      earned <- didEarnDrownedCity TidalFlipMinigame
      completeTheDrownedQuarter
      earned `refShouldBe` False

    it "is not earned with an unrevealed location" . gameTest $ \_ -> do
      asTheDrownedCityScenario "11536"
      _ <- testLocationWith (revealedL .~ False)
      earned <- didEarnDrownedCity TidalFlipMinigame
      completeTheDrownedQuarter
      earned `refShouldBe` False

    it "is not earned merely by the scenario ending" . gameTest $ \_ -> do
      asTheDrownedCityScenario "11536"
      _ <- testLocationWith (revealedL .~ True)
      earned <- didEarnDrownedCity TidalFlipMinigame
      run $ EndOfGame Nothing
      earned `refShouldBe` False

  context "No Acolyte Left Behind" $ do
    -- Rescuing a pilgrim puts their Cultist card underneath the act.
    let rescue n = do
          cards <- replicateM n (genCard Enemies.pilgrimAcolyte)
          run $ PlaceUnderneath (ActTarget $ ActId "11556") cards

    it "is earned after five cultists are rescued" . gameTest $ \_ -> do
      asTheDrownedCityScenario "11553"
      earned <- didEarnDrownedCity NoAcolyteLeftBehind
      rescue 4
      earned `refShouldBe` False
      rescue 1
      earned `refShouldBe` True

    it "is not earned after four" . gameTest $ \_ -> do
      asTheDrownedCityScenario "11553"
      earned <- didEarnDrownedCity NoAcolyteLeftBehind
      rescue 4
      earned `refShouldBe` False

  context "Kill the Adds" $ do
    let defeatMother = do
          location <- testLocation
          mother <- testEnemyWithDef Enemies.mother id
          mother `spawnAt` location
          pure mother

    it "is earned when Mother is never damaged directly" . gameTest $ \_ -> do
      asTheDrownedCityScenario "11553"
      mother <- defeatMother
      earned <- didEarnDrownedCity KillTheAdds
      run $ Defeated (toTarget mother) (toCardId mother) (TestSource mempty) []
      earned `refShouldBe` True

    it "is not earned once Mother has taken real damage" . gameTest $ \_ -> do
      asTheDrownedCityScenario "11553"
      mother <- defeatMother
      earned <- didEarnDrownedCity KillTheAdds
      run $ Damaged (toTarget mother) (nonAttack Nothing (TestSource mempty) 1)
      run $ Defeated (toTarget mother) (toCardId mother) (TestSource mempty) []
      earned `refShouldBe` False

  context "In The Deep End" $ do
    -- testAssetWithDef leaves an asset Unplaced with no controller, so the
    -- Artifact has to be put under the investigator's control explicitly.
    let controlled i = (Asset.controllerL ?~ toId i) . (Asset.placementL .~ InPlayArea (toId i))

    it "is earned holding the Tidal Tablet with the vault fully flooded" . gameTest $ \self -> do
      asTheDrownedCityScenario "11587"
      _ <- testAssetWithDef Assets.tidalTablet (controlled self) self
      location <- testLocation
      run $ SetFloodLevel (toId location) FullyFlooded
      earned <- didEarnDrownedCity InTheDeepEnd
      run $ EndOfGame Nothing
      earned `refShouldBe` True

    it "is not earned with water left to rise" . gameTest $ \self -> do
      asTheDrownedCityScenario "11587"
      _ <- testAssetWithDef Assets.tidalTablet (controlled self) self
      location <- testLocation
      run $ SetFloodLevel (toId location) PartiallyFlooded
      earned <- didEarnDrownedCity InTheDeepEnd
      run $ EndOfGame Nothing
      earned `refShouldBe` False

    it "is not earned without the Tidal Tablet" . gameTest $ \_ -> do
      asTheDrownedCityScenario "11587"
      location <- testLocation
      run $ SetFloodLevel (toId location) FullyFlooded
      earned <- didEarnDrownedCity InTheDeepEnd
      run $ EndOfGame Nothing
      earned `refShouldBe` False

  context "Sorry, Didn't See You There" $ do
    -- Act 1 advancing is what spawns the Tyrant; act 2 advancing is what beats
    -- the scenario (its objective is "all undefeated investigators resigned",
    -- so everyone is eliminated by then).
    let spawnTheTyrant = run $ advanceActMsg Acts.stepsOfGiants
        beatTheTower = run $ advanceActMsg Acts.escapeTheTowerV1
        moveTheGreatLift = do
          lift' <- testLocationWithDef Locations.greatLiftActive id
          run $ PlaceGrid (GridLocation (Pos 1 1) (toId lift'))

    it "is earned beating the scenario with the lift left alone" . gameTest $ \_ -> do
      asTheDrownedCityScenario "11612"
      spawnTheTyrant
      earned <- didEarnDrownedCity SorryDidntSeeYouThere
      beatTheTower
      earned `refShouldBe` True

    -- The winning act advance happens with every investigator resigned, so the
    -- earn must not depend on anyone still being uneliminated.
    it "is earned even though every investigator has resigned" . gameTest $ \self -> do
      asTheDrownedCityScenario "11612"
      spawnTheTyrant
      run $ Resign (toId self)
      earned <- didEarnDrownedCity SorryDidntSeeYouThere
      beatTheTower
      earned `refShouldBe` True

    it "is not earned when the lift moves after the Tyrant spawns" . gameTest $ \_ -> do
      asTheDrownedCityScenario "11612"
      spawnTheTyrant
      moveTheGreatLift
      earned <- didEarnDrownedCity SorryDidntSeeYouThere
      beatTheTower
      earned `refShouldBe` False

    it "is still earned when the lift moves before the Tyrant spawns" . gameTest $ \_ -> do
      asTheDrownedCityScenario "11612"
      moveTheGreatLift
      spawnTheTyrant
      earned <- didEarnDrownedCity SorryDidntSeeYouThere
      beatTheTower
      earned `refShouldBe` True

    -- The spawn is latched, so killing the Tyrant must not reopen the window.
    it "is not earned when the lift moves after the Tyrant is defeated" . gameTest $ \_ -> do
      asTheDrownedCityScenario "11612"
      location <- testLocation
      tyrant <- testEnemyWithDef Enemies.colossalTyrant id
      tyrant `spawnAt` location
      spawnTheTyrant
      run $ Defeated (toTarget tyrant) (toCardId tyrant) (TestSource mempty) []
      moveTheGreatLift
      earned <- didEarnDrownedCity SorryDidntSeeYouThere
      beatTheTower
      earned `refShouldBe` False

  context "Sky Rider" $ do
    it "is earned after five turns end in open sky" . gameTest $ \self -> do
      asTheDrownedCityScenario "11639"
      -- Investigators cannot normally enter open sky, so place directly.
      sky <- testLocationWithDef Locations.openSky id
      run $ PlaceInvestigator (toId self) (AtLocation $ toId sky)
      earned <- didEarnDrownedCity SkyRider
      replicateM_ 4 $ run $ EndTurn (toId self)
      earned `refShouldBe` False
      run $ EndTurn (toId self)
      earned `refShouldBe` True

    it "does not count turns ended elsewhere" . gameTest $ \self -> do
      asTheDrownedCityScenario "11639"
      ground <- testLocation
      run $ PlaceInvestigator (toId self) (AtLocation $ toId ground)
      earned <- didEarnDrownedCity SkyRider
      replicateM_ 5 $ run $ EndTurn (toId self)
      earned `refShouldBe` False

  context "Skip to the End" $ do
    it "is earned defeating Cthulhu in Sepulchre of the Sleeper" . gameTest $ \_ -> do
      asTheDrownedCityScenario "11673"
      location <- testLocation
      cthulhu <- testEnemyWithDef Enemies.cthulhuDeadAndDreaming id
      cthulhu `spawnAt` location
      earned <- didEarnDrownedCity SkipToTheEnd
      run $ Defeated (toTarget cthulhu) (toCardId cthulhu) (TestSource mempty) []
      earned `refShouldBe` True

    it "is not earned by defeating anything else" . gameTest $ \_ -> do
      asTheDrownedCityScenario "11673"
      location <- testLocation
      enemy <- testEnemy
      enemy `spawnAt` location
      earned <- didEarnDrownedCity SkipToTheEnd
      run $ Defeated (toTarget enemy) (toCardId enemy) (TestSource mempty) []
      earned `refShouldBe` False

  context "alien glyphs" $ do
    let translate letters =
          run
            $ RecordSetInsert (toCampaignLogKey DiscoveredGlyphs)
            $ map (recorded . String . T.singleton) letters

    it "earns Alien School Graduate at twenty-six glyphs" . gameTest $ \_ -> do
      asTheDrownedCity
      earned <- didEarnDrownedCity AlienSchoolGraduate
      translate ['A' .. 'Y']
      earned `refShouldBe` False
      translate ['Z']
      earned `refShouldBe` True

    it "does not earn Alien School Graduate at twenty-five" . gameTest $ \_ -> do
      asTheDrownedCity
      earned <- didEarnDrownedCity AlienSchoolGraduate
      translate ['A' .. 'Y']
      earned `refShouldBe` False

    it "earns Alien School Dropout when nothing was translated" . gameTest $ \_ -> do
      asTheDrownedCity
      earned <- didEarnDrownedCity AlienSchoolDropout
      finishTheCampaign
      earned `refShouldBe` True

    it "does not earn Alien School Dropout after a single glyph" . gameTest $ \_ -> do
      asTheDrownedCity
      earned <- didEarnDrownedCity AlienSchoolDropout
      translate ['A']
      finishTheCampaign
      earned `refShouldBe` False

  context "Empty Handed" $ do
    it "is earned returning from R'lyeh with no artifact" . gameTest $ \_ -> do
      asTheDrownedCity
      earned <- didEarnDrownedCity EmptyHanded
      run $ CampaignStep (InterludeStep 4 Nothing)
      earned `refShouldBe` True

    it "is not earned having earned an artifact" . gameTest $ \_ -> do
      asTheDrownedCity
      record BarrierNode
      earned <- didEarnDrownedCity EmptyHanded
      run $ CampaignStep (InterludeStep 4 Nothing)
      earned `refShouldBe` False

  context "WHY. WON'T. YOU. STAY. DEAD?!" $ do
    let victoryInescapable = do
          location <- testLocation
          creature <- testEnemyWithDef Enemies.theInescapable id
          creature `spawnAt` location
          run $ AddToVictory Nothing (toTarget creature)

    it "is earned on the twentieth victory display trip" . gameTest $ \_ -> do
      asTheDrownedCity
      earned <- didEarnDrownedCity WhyWontYouStayDead
      replicateM_ 19 victoryInescapable
      earned `refShouldBe` False
      victoryInescapable
      earned `refShouldBe` True

    it "is not earned after nineteen" . gameTest $ \_ -> do
      asTheDrownedCity
      earned <- didEarnDrownedCity WhyWontYouStayDead
      replicateM_ 19 victoryInescapable
      earned `refShouldBe` False

  context "With Your Powers Combined…" $ do
    it "reports every earned artifact at the epilogue" . gameTest $ \_ -> do
      asTheDrownedCity
      traverse_ record [BarrierNode, GrislyMask, ObsidianClaw, TidalTablet, ShardOfYchlecht, HorrorInClay]
      progressed <-
        didProgressDrownedCity
          WithYourPowersCombined
          ["BarrierNode", "GrislyMask", "ObsidianClaw", "TidalTablet", "ShardOfYchlecht", "HorrorInClay"]
      finishTheCampaign
      progressed `refShouldBe` True

    it "reports only the artifacts actually earned" . gameTest $ \_ -> do
      asTheDrownedCity
      traverse_ record [GrislyMask, TidalTablet]
      progressed <- didProgressDrownedCity WithYourPowersCombined ["GrislyMask", "TidalTablet"]
      finishTheCampaign
      progressed `refShouldBe` True

  context "Obligations" $ do
    it "reports a completed Task at the epilogue" . gameTest $ \self -> do
      asTheDrownedCity
      run $ RecordForInvestigator (toId self) (toCampaignLogKey IsStrongInTheirFaith)
      progressed <- didProgressDrownedCity Obligations ["WalkInFaith"]
      finishTheCampaign
      progressed `refShouldBe` True

    it "reports nothing when no Task was completed" . gameTest $ \_ -> do
      asTheDrownedCity
      progressed <- didProgressDrownedCity Obligations ["WalkInFaith"]
      finishTheCampaign
      progressed `refShouldBe` False

  context "winning the campaign" $ do
    it "earns Line in the Sand with three active ultimatums" . gameTest $ \_ -> do
      asTheDrownedCity
      withUltimatums [UltimatumOfFailure, UltimatumOfHardship, UltimatumOfDread]
      earned <- didEarnDrownedCity DrownedCityLineInTheSand
      finishTheCampaign
      earned `refShouldBe` True

    it "does not earn Line in the Sand with only two" . gameTest $ \_ -> do
      asTheDrownedCity
      withUltimatums [UltimatumOfFailure, UltimatumOfHardship]
      earned <- didEarnDrownedCity DrownedCityLineInTheSand
      finishTheCampaign
      earned `refShouldBe` False

    it "earns R'lyeh Expertise on Expert" . gameTest $ \_ -> do
      asTheDrownedCityWith Expert
      earned <- didEarnDrownedCity RlyehExpertise
      finishTheCampaign
      earned `refShouldBe` True

    it "does not earn R'lyeh Expertise below Expert" . gameTest $ \_ -> do
      asTheDrownedCityWith Hard
      earned <- didEarnDrownedCity RlyehExpertise
      finishTheCampaign
      earned `refShouldBe` False

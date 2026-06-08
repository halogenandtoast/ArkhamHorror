module Arkham.Asset.Assets.TheMuscleUnpracticedSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Modifier
import Arkham.Projection
import Arkham.Token
import Arkham.Trait (Trait (Casino))
import TestImport.New

defeatCasinoEnemyAt :: Investigator -> Location -> TestAppT ()
defeatCasinoEnemyAt self location = do
  enemy <- testEnemy & prop @"health" 1 & prop @"fight" 1
  run =<< gameModifier (TestSource mempty) (toTarget enemy) (AddTrait Casino)
  enemy `spawnAt` location
  setChaosTokens [Zero]
  void $ self `fightEnemy` enemy
  startSkillTest
  click "Apply results"

spec :: Spec
spec = describe "The Muscle (Unpracticed)" do
  context "after you defeat a Casino enemy" do
    it "reduces your alarm level by 2 if there are no other Casino enemies at your location or a connecting location" . gameTest $ \self -> do
      self `putCardIntoPlay` Assets.theMuscleUnpracticed
      run $ PlaceTokens (TestSource mempty) (toTarget self) AlarmLevel 3
      (location1, _location2) <- testConnectedLocations id id
      self `moveTo` location1
      defeatCasinoEnemyAt self location1
      useReaction
      fieldMap InvestigatorTokens (countTokens AlarmLevel) (toId self) `shouldReturn` 1

    it "reduces your alarm level by only 1 if another Casino enemy is at a connecting location" . gameTest $ \self -> do
      self `putCardIntoPlay` Assets.theMuscleUnpracticed
      run $ PlaceTokens (TestSource mempty) (toTarget self) AlarmLevel 3
      (location1, location2) <- testConnectedLocations id id
      self `moveTo` location1
      other <- testEnemy
      run =<< gameModifier (TestSource mempty) (toTarget other) (AddTrait Casino)
      other `spawnAt` location2
      defeatCasinoEnemyAt self location1
      useReaction
      fieldMap InvestigatorTokens (countTokens AlarmLevel) (toId self) `shouldReturn` 2

    it "still reduces your alarm level by 2 if only non-Casino enemies are nearby" . gameTest $ \self -> do
      self `putCardIntoPlay` Assets.theMuscleUnpracticed
      run $ PlaceTokens (TestSource mempty) (toTarget self) AlarmLevel 3
      (location1, location2) <- testConnectedLocations id id
      self `moveTo` location1
      other <- testEnemy
      other `spawnAt` location2
      defeatCasinoEnemyAt self location1
      useReaction
      fieldMap InvestigatorTokens (countTokens AlarmLevel) (toId self) `shouldReturn` 1

module Arkham.Enemy.Cards.GuardianOfTheCrystallizerSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import TestImport.New

spec :: Spec
spec = describe "Guardian of the Crystallizer" do
  isHunter Enemies.guardianOfTheCrystallizer

  it "enters play exhausted" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    crystallizerOfDreams <- genCard Assets.crystallizerOfDreams
    guardianOfTheCrystallizer <- genCard Enemies.guardianOfTheCrystallizer
    withProp @"resources" 1 self
    withProp @"bonded" [guardianOfTheCrystallizer] self
    withProp @"hand" [crystallizerOfDreams] self
    duringTurn self do
      self `playCard` crystallizerOfDreams
      self `drawCards` 1
    assertAny $ enemyIs Enemies.guardianOfTheCrystallizer <> ExhaustedEnemy

  it
    "If there is no Crystallizer of Dreams in play: Set Guardian of the Crystallizer aside, out of play."
    . gameTest
    $ \self -> do
      location <- testLocation
      self `moveTo` location
      crystallizerOfDreams <- genMyCard self Assets.crystallizerOfDreams
      guardianOfTheCrystallizer <- genMyCard self Enemies.guardianOfTheCrystallizer
      withProp @"resources" 1 self
      withProp @"bonded" [guardianOfTheCrystallizer] self
      withProp @"hand" [crystallizerOfDreams] self
      duringTurn self do
        self `playCard` crystallizerOfDreams
        crystallizerOfDream <- selectJust $ assetIs Assets.crystallizerOfDreams
        run $ Discard Nothing (TestSource mempty) (toTarget crystallizerOfDream)
        self `drawCards` 1
        useForcedAbility
        assertNone $ enemyIs Enemies.guardianOfTheCrystallizer
        self.discard `shouldReturn` onlyPlayerCards [crystallizerOfDreams]
        self.bonded `shouldReturn` [guardianOfTheCrystallizer]

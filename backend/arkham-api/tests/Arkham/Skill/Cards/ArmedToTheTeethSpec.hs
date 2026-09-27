module Arkham.Skill.Cards.ArmedToTheTeethSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Skill.Cards qualified as Skills
import TestImport.New

spec :: Spec
spec = describe "Armed to the Teeth" $ do
  it "gains two wild icons committed to a test on an Item asset you control" . gameTest $ \self -> do
    withProp @"combat" 1 self
    machete <- self `putAssetIntoPlay` Assets.machete
    armedToTheTeeth <- genCard Skills.armedToTheTeeth
    self `addToHand` armedToTheTeeth
    enemy <- testEnemy & prop @"fight" 2 & prop @"health" 5
    location <- testLocation
    setChaosTokens [Zero]
    run $ placedLocation location
    enemy `spawnAt` location
    self `moveTo` location
    [doFight] <- machete.abilities
    self `useAbility` doFight
    click "choose enemy"
    commit armedToTheTeeth

    -- combat 1 + Machete 1 + printed combat icon 1 + two wilds = 5. The count has
    -- to be right here, during ST.2, while the player is still deciding -- the
    -- Skill entity that carries the modifier does not exist until the test starts
    -- (#5777).
    self.skillValue `shouldReturn` 5

    -- and it must not double once CommitCard builds the real entity alongside the
    -- stand-in from 'pendingCommitEntities'
    startSkillTest
    self.skillValue `shouldReturn` 5

  it "does not gain them on a test that is not on an Item asset" . gameTest $ \self -> do
    withProp @"combat" 1 self
    armedToTheTeeth <- genCard Skills.armedToTheTeeth
    self `addToHand` armedToTheTeeth
    enemy <- testEnemy & prop @"fight" 2 & prop @"health" 5
    location <- testLocation
    setChaosTokens [Zero]
    run $ placedLocation location
    enemy `spawnAt` location
    self `moveTo` location
    _ <- self `fightEnemy` enemy
    commit armedToTheTeeth

    -- combat 1 + printed combat icon 1, no wilds: a basic fight is not a test
    -- "on" a card
    self.skillValue `shouldReturn` 2

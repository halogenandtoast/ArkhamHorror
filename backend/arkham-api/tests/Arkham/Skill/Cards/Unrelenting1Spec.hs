module Arkham.Skill.Cards.Unrelenting1Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.ChaosBag.Base
import Arkham.Helpers.Scenario
import Arkham.Scenario.Types (Field (..))
import Arkham.Skill.Cards qualified as Skills
import Arkham.Target
import TestImport.New

spec :: Spec
spec = describe "Unrelenting (1)" $ do
  it "returns sealed tokens to the bag when committed with On the Brink on a failed test" . gameTest $ \self -> do
    cards <- testPlayerCards 5
    withProp @"deck" (Deck cards) self
    withProp @"combat" 1 self
    onTheBrink <- genCard Skills.onTheBrink
    unrelenting <- genCard Skills.unrelenting1
    self `addToHand` onTheBrink
    self `addToHand` unrelenting
    setChaosTokens [Zero, PlusOne, ElderSign, MinusTwo]

    sid <- getRandom
    run $ beginSkillTest sid self #combat 5
    commit onTheBrink
    commit unrelenting
    startSkillTest

    -- Unrelenting: seal Zero, PlusOne, ElderSign (3 good -> draw 2), then done
    chooseOptionMatching "seal zero" \case
      TargetLabel (ChaosTokenFaceTarget Zero) _ -> True
      _ -> False
    chooseOptionMatching "seal plus one" \case
      TargetLabel (ChaosTokenFaceTarget PlusOne) _ -> True
      _ -> False
    chooseOptionMatching "seal elder sign" \case
      TargetLabel (ChaosTokenFaceTarget ElderSign) _ -> True
      _ -> False

    assertFailedSkillTest
    applyResults

    tokens <- scenarioFieldMap ScenarioChaosBag (map chaosTokenFace . chaosBagChaosTokens)
    liftIO $ tokens `shouldMatchList` [Zero, PlusOne, ElderSign, MinusTwo]

  it "returns sealed tokens to the bag after a passed test (no On the Brink)" . gameTest $ \self -> do
    withProp @"combat" 5 self
    unrelenting <- genCard Skills.unrelenting1
    self `addToHand` unrelenting
    setChaosTokens [Zero, PlusOne, MinusOne]

    sid <- getRandom
    run $ beginSkillTest sid self #combat 1
    commit unrelenting
    startSkillTest

    chooseOptionMatching "seal zero" \case
      TargetLabel (ChaosTokenFaceTarget Zero) _ -> True
      _ -> False
    chooseOptionMatching "seal plus one" \case
      TargetLabel (ChaosTokenFaceTarget PlusOne) _ -> True
      _ -> False
    chooseOptionMatching "done sealing" \case
      Done {} -> True
      _ -> False

    assertPassedSkillTest
    applyResults

    tokens <- scenarioFieldMap ScenarioChaosBag (map chaosTokenFace . chaosBagChaosTokens)
    liftIO $ tokens `shouldMatchList` [Zero, PlusOne, MinusOne]

  it "returns sealed tokens to the bag on an ability-sourced (fight) skill test" . gameTest $ \self -> do
    withProp @"combat" 5 self
    machete <- self `putAssetIntoPlay` Assets.machete
    enemy <- testEnemy & prop @"fight" 1 & prop @"health" 3
    location <- testLocation
    unrelenting <- genCard Skills.unrelenting1
    self `addToHand` unrelenting
    setChaosTokens [Zero, PlusOne, MinusOne]
    enemy `spawnAt` location
    self `moveTo` location

    [doFight] <- self `getActionsFrom` machete
    self `useAbility` doFight
    chooseTarget enemy
    commit unrelenting
    startSkillTest

    chooseOptionMatching "seal zero" \case
      TargetLabel (ChaosTokenFaceTarget Zero) _ -> True
      _ -> False
    chooseOptionMatching "seal plus one" \case
      TargetLabel (ChaosTokenFaceTarget PlusOne) _ -> True
      _ -> False
    chooseOptionMatching "done sealing" \case
      Done {} -> True
      _ -> False

    applyResults

    tokens <- scenarioFieldMap ScenarioChaosBag (map chaosTokenFace . chaosBagChaosTokens)
    liftIO $ tokens `shouldMatchList` [Zero, PlusOne, MinusOne]

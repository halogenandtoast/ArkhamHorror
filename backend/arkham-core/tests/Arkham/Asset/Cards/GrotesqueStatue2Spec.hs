module Arkham.Asset.Cards.GrotesqueStatue2Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.ChaosBag.Base
import Arkham.ChaosBagStepState
import Arkham.Game.Settings
import Arkham.Scenario.Types (Field (..))
import TestImport.New

spec :: Spec
spec = describe "Grotesque Statue (2)" $ do
  context "when would reveal a token" $ do
    it "reveals 2 tokens and let's you choose one" . gameTest $ \self -> do
      withProp @"intellect" 5 self
      self `putCardIntoPlay` Assets.grotesqueStatue2
      setChaosTokens [AutoFail, Zero]

      sid <- getRandom
      run $ beginSkillTest sid self SkillIntellect 0
      startSkillTest
      useReaction

      unlessSetting settingsAbilitiesCannotReactToThemselves $ do
        -- skip grotesque statue reacting to itself
        skip
        skip

      chooseOptionMatching "choose zero token" \case
        ChaosTokenGroupChoice _ _ (ChooseMatch _ 1 _ _ [[ChaosToken _ Zero _]] _) -> True
        _ -> False
      assertPassedSkillTest
      applyResults

      tokens <- scenarioFieldMap ScenarioChaosBag (map chaosTokenFace . chaosBagChaosTokens)
      liftIO $ tokens `shouldMatchList` [Zero, AutoFail]

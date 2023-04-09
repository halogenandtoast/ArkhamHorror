module Arkham.Asset.Cards.GrotesqueStatue4Spec
  ( spec
  ) where

import TestImport

import Arkham.Asset.Cards qualified as Assets
import Arkham.ChaosBagStepState
import Arkham.ChaosBag.Base
import Arkham.Investigator.Types (intellectL)
import Arkham.Scenario.Types (Field(..))

spec :: Spec
spec = describe "Grotesque Statue (4)" $ do
  context "when would reveal a token" $ do
    it "reveals 2 tokens and let's you choose one" $ do
      investigator <- testJenny (intellectL .~ 5)
      grotestqueStatue <- buildAsset Assets.grotesqueStatue4 (Just investigator)

      (didRunMessage, logger) <- didPassSkillTestBy
        investigator
        SkillIntellect
        5

      gameTestWithLogger
          logger
          investigator
          [ SetTokens [AutoFail, Zero]
          , playAsset investigator grotestqueStatue
          , beginSkillTest investigator SkillIntellect 0
          ]
          (entitiesL . assetsL %~ insertEntity grotestqueStatue)
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOptionMatching
              "use ability"
              (\case
                AbilityLabel{} -> True
                _ -> False
              )
            chooseOptionMatching
              "skip use ability"
              (\case
                Label{} -> True
                _ -> False
              )
            chooseOptionMatching
              "skip use ability"
              (\case
                Label{} -> True
                _ -> False
              )
            chooseOptionMatching
              "choose zero token"
              (\case
                TokenGroupChoice _ _ (ChooseMatch _ 1 _ _ [[Token _ Zero]] _) -> True
                _ -> False
              )
            chooseOnlyOption "apply results"

            didRunMessage `refShouldBe` True
            tokens <- scenarioFieldMap ScenarioChaosBag (map tokenFace . chaosBagTokens)
            liftIO $ tokens `shouldMatchList` [Zero, AutoFail]

module Arkham.Types.Asset.Cards.GrotesqueStatue4Spec
  ( spec
  ) where

import TestImport

import Arkham.Types.ChaosBagStepState

spec :: Spec
spec = describe "Grotesque Statue (4)" $ do
  context "when would reveal a token" $ do
    it "reveals 2 tokens and let's you choose one" $ do
      investigator <- testInvestigator "00000" id
      grotestqueStatue <- buildAsset "01071"

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
          (assetsL %~ insertEntity grotestqueStatue)
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOptionMatching
              "use ability"
              (\case
                Run{} -> True
                _ -> False
              )
            chooseOptionMatching
              "choose zero token"
              (\case
                ChooseTokenGroups _ _ (Choose 1 _ [[Token _ Zero]]) -> True
                _ -> False
              )
            chooseOnlyOption "apply results"

            didRunMessage `refShouldBe` True
            getChaosBagTokens `shouldMatchListM` [Zero, AutoFail]

module Arkham.Types.Asset.Cards.GrotesqueStatue4Spec
  ( spec
  )
where

import TestImport

import Arkham.Types.ChaosBagStepState

spec :: Spec
spec = describe "Grotesque Statue (4)" $ do
  context "when would reveal a token" $ do
    it "reveals 2 tokens and let's you choose one" $ do
      investigator <- testInvestigator "00000" id
      grotestqueStatue <- buildAsset "01071"
      runGameTest
          investigator
          [ SetTokens [AutoFail, Zero]
          , playAsset investigator grotestqueStatue
          , beginSkillTest investigator SkillIntellect 0
          ]
          (assetsL %~ insertEntity grotestqueStatue)
        $ do
            (didRunMessage, logger) <- didPassSkillTestBy
              investigator
              SkillIntellect
              5
            runMessagesNoLogging
            runGameTestOnlyOption "start skill test"
            runGameTestOptionMatching
              "use ability"
              (\case
                Run{} -> True
                _ -> False
              )
            runGameTestOptionMatching
              "choose zero token"
              (\case
                ChooseTokenGroups _ _ (Choose 1 _ [[Zero]]) -> True
                _ -> False
              )
            runGameTestOnlyOptionWithLogger "apply results" logger

            didRunMessage `refShouldBe` True
            getChaosBagTokens `shouldMatchListM` [Zero, AutoFail]

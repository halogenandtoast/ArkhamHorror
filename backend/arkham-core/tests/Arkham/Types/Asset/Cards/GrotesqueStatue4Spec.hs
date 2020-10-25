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
      scenario' <- testScenario "00000" id
      game <-
        runGameTest
          investigator
          [ SetTokens [AutoFail, Zero]
          , playAsset investigator grotestqueStatue
          , beginSkillTest investigator SkillIntellect 0
          ]
          ((scenario ?~ scenario') . (assets %~ insertEntity grotestqueStatue))
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOptionMatching
              "use ability"
              (\case
                Run{} -> True
                _ -> False
              )
        >>= runGameTestOptionMatching
              "choose zero token"
              (\case
                ChooseTokenGroups _ _ (Choose 1 _ [[Zero]]) -> True
                _ -> False
              )
        >>= runGameTestOnlyOption "apply results"

      game `shouldSatisfy` hasProcessedMessage
        (PassedSkillTest
          (getId () investigator)
          Nothing
          TestSource
          SkillTestInitiatorTarget
          5
        )
      chaosBagTokensOf game `shouldMatchList` [Zero, AutoFail]

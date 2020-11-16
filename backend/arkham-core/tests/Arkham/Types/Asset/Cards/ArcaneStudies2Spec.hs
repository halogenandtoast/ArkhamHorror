module Arkham.Types.Asset.Cards.ArcaneStudies2Spec
  ( spec
  )
where

import TestImport

import Arkham.Types.Investigator.Attrs (Attrs(..))

spec :: Spec
spec = describe "Arcane Studies (2)" $ do
  it "Adds 1 to willpower check for each resource spent" $ do
    arcaneStudies2 <- buildAsset "50007"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorWillpower = 1, investigatorResources = 2 }
    scenario' <- testScenario "00000" id
    game <-
      runGameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator arcaneStudies2
        , beginSkillTest investigator SkillWillpower 3
        ]
        ((assets %~ insertEntity arcaneStudies2) . (scenario ?~ scenario'))
      >>= runGameTestOptionMatching
            "use ability"
            (\case
              Run{} -> True
              _ -> False
            )
      >>= runGameTestOptionMatching
            "use ability"
            (\case
              Run{} -> True
              _ -> False
            )
      >>= runGameTestOptionMatching
            "start skill test"
            (\case
              StartSkillTest{} -> True
              _ -> False
            )
      >>= runGameTestOnlyOption "apply results"
    investigator `shouldSatisfy` hasPassedSkillTestBy 0 game TestTarget

  it "Adds 1 to intellect check for each resource spent" $ do
    arcaneStudies2 <- buildAsset "50007"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorIntellect = 1, investigatorResources = 2 }
    scenario' <- testScenario "00000" id
    game <-
      runGameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator arcaneStudies2
        , beginSkillTest investigator SkillIntellect 3
        ]
        ((assets %~ insertEntity arcaneStudies2) . (scenario ?~ scenario'))
      >>= runGameTestOptionMatching
            "use ability"
            (\case
              Run{} -> True
              _ -> False
            )
      >>= runGameTestOptionMatching
            "use ability"
            (\case
              Run{} -> True
              _ -> False
            )
      >>= runGameTestOptionMatching
            "start skill test"
            (\case
              StartSkillTest{} -> True
              _ -> False
            )
      >>= runGameTestOnlyOption "apply results"
    investigator `shouldSatisfy` hasPassedSkillTestBy 0 game TestTarget

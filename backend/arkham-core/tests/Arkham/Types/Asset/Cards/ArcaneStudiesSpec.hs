module Arkham.Types.Asset.Cards.ArcaneStudiesSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Investigator.Attrs (Attrs(..))

spec :: Spec
spec = describe "Arcane Studies" $ do
  it "Adds 1 to willpower check for each resource spent" $ do
    arcaneStudies <- buildAsset "01062"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorWillpower = 1, investigatorResources = 2 }
    scenario' <- testScenario "00000" id
    game <-
      runGameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator arcaneStudies
        , beginSkillTest investigator SkillWillpower 3
        ]
        ((assets %~ insertEntity arcaneStudies) . (scenario ?~ scenario'))
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
    arcaneStudies <- buildAsset "01062"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorIntellect = 1, investigatorResources = 2 }
    scenario' <- testScenario "00000" id
    game <-
      runGameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator arcaneStudies
        , beginSkillTest investigator SkillIntellect 3
        ]
        ((assets %~ insertEntity arcaneStudies) . (scenario ?~ scenario'))
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

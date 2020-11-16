module Arkham.Types.Asset.Cards.DigDeepSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Investigator.Attrs (Attrs(..))

spec :: Spec
spec = describe "Dig Deep" $ do
  it "Adds 1 to willpower check for each resource spent" $ do
    digDeep <- buildAsset "01077"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorWillpower = 1, investigatorResources = 2 }
    scenario' <- testScenario "00000" id
    game <-
      runGameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator digDeep
        , beginSkillTest investigator SkillWillpower 3
        ]
        ((assets %~ insertEntity digDeep) . (scenario ?~ scenario'))
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

  it "Adds 1 to agility check for each resource spent" $ do
    digDeep <- buildAsset "01077"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorAgility = 1, investigatorResources = 2 }
    scenario' <- testScenario "00000" id
    game <-
      runGameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator digDeep
        , beginSkillTest investigator SkillAgility 3
        ]
        ((assets %~ insertEntity digDeep) . (scenario ?~ scenario'))
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

module Arkham.Types.Asset.Cards.HyperawarenessSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Investigator.Attrs (Attrs(..))

spec :: Spec
spec = describe "Hyperawareness" $ do
  it "Adds 1 to intellect check for each resource spent" $ do
    hyperawareness <- buildAsset "01034"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorIntellect = 1, investigatorResources = 2 }
    scenario' <- testScenario "00000" id
    game <-
      runGameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator hyperawareness
        , beginSkillTest investigator SkillIntellect 3
        ]
        ((assets %~ insertEntity hyperawareness) . (scenario ?~ scenario'))
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
    investigator `shouldSatisfy` hasPassedSkillTestBy 0 game

  it "Adds 1 to agility check for each resource spent" $ do
    hyperawareness <- buildAsset "01034"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorAgility = 1, investigatorResources = 2 }
    scenario' <- testScenario "00000" id
    game <-
      runGameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator hyperawareness
        , beginSkillTest investigator SkillAgility 3
        ]
        ((assets %~ insertEntity hyperawareness) . (scenario ?~ scenario'))
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
    investigator `shouldSatisfy` hasPassedSkillTestBy 0 game

module Arkham.Types.Asset.Cards.Hyperawareness2Spec
  ( spec
  )
where

import TestImport

import Arkham.Types.Investigator.Attrs (Attrs(..))
import Arkham.Types.Token

spec :: Spec
spec = describe "Hyperawareness (2)" $ do
  it "Adds 1 to intellect check for each resource spent" $ do
    hyperawareness2 <- buildAsset "50003"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorIntellect = 1, investigatorResources = 2 }
    scenario' <- testScenario "00000" id
    game <-
      runGameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator hyperawareness2
        , beginSkillTest investigator SkillIntellect 3
        ]
        ((assets %~ insertEntity hyperawareness2) . (scenario ?~ scenario'))
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
    game `shouldSatisfy` hasProcessedMessage
      (PassedSkillTest "00000" Nothing TestSource (TokenTarget Zero) 0)

  it "Adds 1 to agility check for each resource spent" $ do
    hyperawareness2 <- buildAsset "50003"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorAgility = 1, investigatorResources = 2 }
    scenario' <- testScenario "00000" id
    game <-
      runGameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator hyperawareness2
        , beginSkillTest investigator SkillAgility 3
        ]
        ((assets %~ insertEntity hyperawareness2) . (scenario ?~ scenario'))
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
    game `shouldSatisfy` hasProcessedMessage
      (PassedSkillTest "00000" Nothing TestSource (TokenTarget Zero) 0)

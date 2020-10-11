module Arkham.Types.Asset.Cards.HardKnocksSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Investigator.Attrs (Attrs(..))
import Arkham.Types.Token

spec :: Spec
spec = describe "Hard Knocks" $ do
  it "Adds 1 to combat check for each resource spent" $ do
    hardKnocks <- buildAsset "01049"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorCombat = 1, investigatorResources = 2 }
    scenario' <- testScenario "00000" id
    game <-
      runGameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator hardKnocks
        , beginSkillTest investigator SkillCombat 3
        ]
        ((assets %~ insertEntity hardKnocks) . (scenario ?~ scenario'))
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
    hardKnocks <- buildAsset "01049"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorAgility = 1, investigatorResources = 2 }
    scenario' <- testScenario "00000" id
    game <-
      runGameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator hardKnocks
        , beginSkillTest investigator SkillAgility 3
        ]
        ((assets %~ insertEntity hardKnocks) . (scenario ?~ scenario'))
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

module Arkham.Types.Asset.Cards.HardKnocks2Spec
  ( spec
  )
where

import TestImport

import Arkham.Types.Investigator.Attrs (Attrs(..))

spec :: Spec
spec = describe "Hard Knocks (2)" $ do
  it "Adds 1 to combat check for each resource spent" $ do
    hardKnocks2 <- buildAsset "50005"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorCombat = 1, investigatorResources = 2 }
    scenario' <- testScenario "00000" id
    (didPassTest, logger) <- didPassSkillTestBy investigator 0
    void
      $ runGameTest
          investigator
          [ SetTokens [Zero]
          , playAsset investigator hardKnocks2
          , beginSkillTest investigator SkillCombat 3
          ]
          ((assets %~ insertEntity hardKnocks2) . (scenario ?~ scenario'))
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
      >>= runGameTestOnlyOptionWithLogger "apply results" logger
    readIORef didPassTest `shouldReturn` True

  it "Adds 1 to agility check for each resource spent" $ do
    hardKnocks2 <- buildAsset "50005"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorAgility = 1, investigatorResources = 2 }
    scenario' <- testScenario "00000" id
    (didPassTest, logger) <- didPassSkillTestBy investigator 0
    void
      $ runGameTest
          investigator
          [ SetTokens [Zero]
          , playAsset investigator hardKnocks2
          , beginSkillTest investigator SkillAgility 3
          ]
          ((assets %~ insertEntity hardKnocks2) . (scenario ?~ scenario'))
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
      >>= runGameTestOnlyOptionWithLogger "apply results" logger
    readIORef didPassTest `shouldReturn` True

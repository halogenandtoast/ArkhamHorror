module Arkham.Types.Asset.Cards.DigDeep2Spec
  ( spec
  )
where

import TestImport

import Arkham.Types.Investigator.Attrs (Attrs(..))

spec :: Spec
spec = describe "Dig Deep (2)" $ do
  it "Adds 1 to willpower check for each resource spent" $ do
    digDeep2 <- buildAsset "50009"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorWillpower = 1, investigatorResources = 2 }
    scenario' <- testScenario "00000" id
    (didPassTest, logger) <- didPassSkillTestBy investigator 0
    void
      $ runGameTest
          investigator
          [ SetTokens [Zero]
          , playAsset investigator digDeep2
          , beginSkillTest investigator SkillWillpower 3
          ]
          ((assets %~ insertEntity digDeep2) . (scenario ?~ scenario'))
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
    digDeep2 <- buildAsset "50009"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorAgility = 1, investigatorResources = 2 }
    scenario' <- testScenario "00000" id
    (didPassTest, logger) <- didPassSkillTestBy investigator 0
    void
      $ runGameTest
          investigator
          [ SetTokens [Zero]
          , playAsset investigator digDeep2
          , beginSkillTest investigator SkillAgility 3
          ]
          ((assets %~ insertEntity digDeep2) . (scenario ?~ scenario'))
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

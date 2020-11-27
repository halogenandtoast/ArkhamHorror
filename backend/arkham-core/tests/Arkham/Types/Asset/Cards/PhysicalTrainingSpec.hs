module Arkham.Types.Asset.Cards.PhysicalTrainingSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Investigator.Attrs (Attrs(..))

spec :: Spec
spec = describe "Physical Training" $ do
  it "Adds 1 to willpower check for each resource spent" $ do
    physicalTraining <- buildAsset "01017"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorWillpower = 1, investigatorResources = 2 }
    (didPassTest, logger) <- didPassSkillTestBy investigator 0
    void
      $ runGameTest
          investigator
          [ SetTokens [Zero]
          , playAsset investigator physicalTraining
          , beginSkillTest investigator SkillWillpower 3
          ]
          (assets %~ insertEntity physicalTraining)
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

  it "Adds 1 to combat check for each resource spent" $ do
    physicalTraining <- buildAsset "01017"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorCombat = 1, investigatorResources = 2 }
    (didPassTest, logger) <- didPassSkillTestBy investigator 0
    void
      $ runGameTest
          investigator
          [ SetTokens [Zero]
          , playAsset investigator physicalTraining
          , beginSkillTest investigator SkillCombat 3
          ]
          (assets %~ insertEntity physicalTraining)
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

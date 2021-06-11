module Arkham.Types.Asset.Cards.PhysicalTraining2Spec
  ( spec
  )
where

import TestImport

import Arkham.Types.Investigator.Attrs (InvestigatorAttrs(..))

spec :: Spec
spec = describe "Physical Training (2)" $ do
  it "Adds 1 to willpower check for each resource spent" $ do
    physicalTraining2 <- buildAsset "50001"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorWillpower = 1, investigatorResources = 2 }
    runGameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator physicalTraining2
        , beginSkillTest investigator SkillWillpower 3
        ]
        (assetsL %~ insertEntity physicalTraining2)
      $ do
          (didPassTest, logger) <- didPassSkillTestBy
            investigator
            SkillWillpower
            0
          runMessagesNoLogging
          runGameTestOptionMatching
            "use ability"
            (\case
              Run{} -> True
              _ -> False
            )
          runGameTestOptionMatching
            "use ability"
            (\case
              Run{} -> True
              _ -> False
            )
          runGameTestOptionMatching
            "start skill test"
            (\case
              StartSkillTest{} -> True
              _ -> False
            )
          runGameTestOnlyOptionWithLogger "apply results" logger
          didPassTest `refShouldBe` True

  it "Adds 1 to combat check for each resource spent" $ do
    physicalTraining2 <- buildAsset "50001"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorCombat = 1, investigatorResources = 2 }
    runGameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator physicalTraining2
        , beginSkillTest investigator SkillCombat 3
        ]
        (assetsL %~ insertEntity physicalTraining2)
      $ do
          (didPassTest, logger) <- didPassSkillTestBy investigator SkillCombat 0
          runMessagesNoLogging
          runGameTestOptionMatching
            "use ability"
            (\case
              Run{} -> True
              _ -> False
            )
          runGameTestOptionMatching
            "use ability"
            (\case
              Run{} -> True
              _ -> False
            )
          runGameTestOptionMatching
            "start skill test"
            (\case
              StartSkillTest{} -> True
              _ -> False
            )
          runGameTestOnlyOptionWithLogger "apply results" logger
          didPassTest `refShouldBe` True

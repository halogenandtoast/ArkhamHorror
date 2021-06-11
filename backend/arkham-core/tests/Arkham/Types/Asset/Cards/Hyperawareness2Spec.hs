module Arkham.Types.Asset.Cards.Hyperawareness2Spec
  ( spec
  )
where

import TestImport

import Arkham.Types.Investigator.Attrs (InvestigatorAttrs(..))

spec :: Spec
spec = describe "Hyperawareness (2)" $ do
  it "Adds 1 to intellect check for each resource spent" $ do
    hyperawareness2 <- buildAsset "50003"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorIntellect = 1, investigatorResources = 2 }
    runGameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator hyperawareness2
        , beginSkillTest investigator SkillIntellect 3
        ]
        (assetsL %~ insertEntity hyperawareness2)
      $ do
          (didPassTest, logger) <- didPassSkillTestBy
            investigator
            SkillIntellect
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

  it "Adds 1 to agility check for each resource spent" $ do
    hyperawareness2 <- buildAsset "50003"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorAgility = 1, investigatorResources = 2 }
    runGameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator hyperawareness2
        , beginSkillTest investigator SkillAgility 3
        ]
        (assetsL %~ insertEntity hyperawareness2)
      $ do
          (didPassTest, logger) <- didPassSkillTestBy
            investigator
            SkillAgility
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

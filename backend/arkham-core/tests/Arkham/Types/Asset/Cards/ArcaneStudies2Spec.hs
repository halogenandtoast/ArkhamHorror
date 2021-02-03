module Arkham.Types.Asset.Cards.ArcaneStudies2Spec
  ( spec
  ) where

import TestImport

import Arkham.Types.Investigator.Attrs (InvestigatorAttrs(..))

spec :: Spec
spec = describe "Arcane Studies (2)" $ do
  it "Adds 1 to willpower check for each resource spent" $ do
    arcaneStudies2 <- buildAsset "50007"
    investigator <- testInvestigator "00000" $ \attrs ->
      attrs { investigatorWillpower = 1, investigatorResources = 2 }
    (didPassTest, logger) <- didPassSkillTestBy investigator SkillWillpower 0
    void
      $ runGameTest
          investigator
          [ SetTokens [Zero]
          , playAsset investigator arcaneStudies2
          , beginSkillTest investigator SkillWillpower 3
          ]
          (assetsL %~ insertEntity arcaneStudies2)
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

  it "Adds 1 to intellect check for each resource spent" $ do
    arcaneStudies2 <- buildAsset "50007"
    investigator <- testInvestigator "00000" $ \attrs ->
      attrs { investigatorIntellect = 1, investigatorResources = 2 }
    (didPassTest, logger) <- didPassSkillTestBy investigator SkillIntellect 0
    void
      $ runGameTest
          investigator
          [ SetTokens [Zero]
          , playAsset investigator arcaneStudies2
          , beginSkillTest investigator SkillIntellect 3
          ]
          (assetsL %~ insertEntity arcaneStudies2)
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

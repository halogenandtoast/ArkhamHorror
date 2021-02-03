module Arkham.Types.Asset.Cards.ArcaneStudiesSpec
  ( spec
  ) where

import TestImport

import Arkham.Types.Investigator.Attrs (InvestigatorAttrs(..))

spec :: Spec
spec = describe "Arcane Studies" $ do
  it "Adds 1 to willpower check for each resource spent" $ do
    arcaneStudies <- buildAsset "01062"
    investigator <- testInvestigator "00000" $ \attrs ->
      attrs { investigatorWillpower = 1, investigatorResources = 2 }
    (didPassTest, logger) <- didPassSkillTestBy investigator SkillWillpower 0
    void
      $ runGameTest
          investigator
          [ SetTokens [Zero]
          , playAsset investigator arcaneStudies
          , beginSkillTest investigator SkillWillpower 3
          ]
          (assetsL %~ insertEntity arcaneStudies)
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
    arcaneStudies <- buildAsset "01062"
    investigator <- testInvestigator "00000" $ \attrs ->
      attrs { investigatorIntellect = 1, investigatorResources = 2 }
    (didPassTest, logger) <- didPassSkillTestBy investigator SkillIntellect 0
    void
      $ runGameTest
          investigator
          [ SetTokens [Zero]
          , playAsset investigator arcaneStudies
          , beginSkillTest investigator SkillIntellect 3
          ]
          (assetsL %~ insertEntity arcaneStudies)
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

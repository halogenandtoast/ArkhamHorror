module Arkham.Types.Asset.Cards.ArcaneStudies2Spec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Types.Ability
import Arkham.Types.Investigator.Attrs (InvestigatorAttrs(..))

spec :: Spec
spec = describe "Arcane Studies (2)" $ do
  it "Adds 1 to willpower check for each resource spent" $ do
    arcaneStudies2 <- buildAsset "50007"
    investigator <- testInvestigator "00000" $ \attrs ->
      attrs { investigatorWillpower = 1, investigatorResources = 2 }
    (didPassTest, logger) <- didPassSkillTestBy investigator SkillWillpower 0
    gameTestWithLogger
        logger
        investigator
        [ SetTokens [Zero]
        , playAsset investigator arcaneStudies2
        , beginSkillTest investigator SkillWillpower 3
        ]
        (assetsL %~ insertEntity arcaneStudies2)
      $ do
          runMessages
          chooseOptionMatching
            "use ability"
            (\case
              Run (x : _) -> case x of
                UseAbility _ a _ -> abilityIndex a == 1
                _ -> False
              _ -> False
            )
          chooseOptionMatching
            "use ability"
            (\case
              Run (x : _) -> case x of
                UseAbility _ a _ -> abilityIndex a == 1
                _ -> False
              _ -> False
            )
          chooseOptionMatching
            "start skill test"
            (\case
              StartSkillTest{} -> True
              _ -> False
            )
          chooseOnlyOption "apply results"
          didPassTest `refShouldBe` True

  it "Adds 1 to intellect check for each resource spent" $ do
    arcaneStudies2 <- buildAsset "50007"
    investigator <- testInvestigator "00000" $ \attrs ->
      attrs { investigatorIntellect = 1, investigatorResources = 2 }

    (didPassTest, logger) <- didPassSkillTestBy investigator SkillIntellect 0
    gameTestWithLogger
        logger
        investigator
        [ SetTokens [Zero]
        , playAsset investigator arcaneStudies2
        , beginSkillTest investigator SkillIntellect 3
        ]
        (assetsL %~ insertEntity arcaneStudies2)
      $ do
          runMessages
          chooseOptionMatching
            "use ability"
            (\case
              Run (x : _) -> case x of
                UseAbility _ a _ -> abilityIndex a == 2
                _ -> False
              _ -> False
            )
          chooseOptionMatching
            "use ability"
            (\case
              Run (x : _) -> case x of
                UseAbility _ a _ -> abilityIndex a == 2
                _ -> False
              _ -> False
            )
          chooseOptionMatching
            "start skill test"
            (\case
              StartSkillTest{} -> True
              _ -> False
            )
          chooseOnlyOption "apply results"
          didPassTest `refShouldBe` True

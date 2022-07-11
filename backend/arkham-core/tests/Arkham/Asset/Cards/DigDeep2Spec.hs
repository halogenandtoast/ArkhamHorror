module Arkham.Asset.Cards.DigDeep2Spec (
  spec,
) where

import TestImport

import Arkham.Ability
import Arkham.Investigator.Attrs (InvestigatorAttrs(..))

spec :: Spec
spec = describe "Dig Deep (2)" $ do
  it "Adds 1 to willpower check for each resource spent" $ do
    investigator <- testInvestigator $ \attrs ->
      attrs {investigatorWillpower = 1, investigatorResources = 2}

    digDeep2 <- buildAsset "50009" (Just investigator)

    (didPassTest, logger) <- didPassSkillTestBy investigator SkillWillpower 0

    gameTestWithLogger
      logger
      investigator
      [ SetTokens [Zero]
      , playAsset investigator digDeep2
      , beginSkillTest investigator SkillWillpower 3
      ]
      (entitiesL . assetsL %~ insertEntity digDeep2)
      $ do
        runMessages
        chooseOptionMatching
          "use ability"
          ( \case
              Run (x : _) -> case x of
                UseAbility _ a _ -> abilityIndex a == 1
                _ -> False
              _ -> False
          )
        chooseOptionMatching
          "use ability"
          ( \case
              Run (x : _) -> case x of
                UseAbility _ a _ -> abilityIndex a == 1
                _ -> False
              _ -> False
          )
        chooseOptionMatching
          "start skill test"
          ( \case
              StartSkillTest {} -> True
              _ -> False
          )
        chooseOnlyOption "apply results"
        didPassTest `refShouldBe` True

  it "Adds 1 to agility check for each resource spent" $ do
    investigator <- testInvestigator $
      \attrs -> attrs {investigatorAgility = 1, investigatorResources = 2}

    digDeep2 <- buildAsset "50009" (Just investigator)

    (didPassTest, logger) <- didPassSkillTestBy investigator SkillAgility 0

    gameTestWithLogger
      logger
      investigator
      [ SetTokens [Zero]
      , playAsset investigator digDeep2
      , beginSkillTest investigator SkillAgility 3
      ]
      (entitiesL . assetsL %~ insertEntity digDeep2)
      $ do
        runMessages
        chooseOptionMatching
          "use ability"
          ( \case
              Run (x : _) -> case x of
                UseAbility _ a _ -> abilityIndex a == 2
                _ -> False
              _ -> False
          )
        chooseOptionMatching
          "use ability"
          ( \case
              Run (x : _) -> case x of
                UseAbility _ a _ -> abilityIndex a == 2
                _ -> False
              _ -> False
          )
        chooseOptionMatching
          "start skill test"
          ( \case
              StartSkillTest {} -> True
              _ -> False
          )
        chooseOnlyOption "apply results"
        didPassTest `refShouldBe` True

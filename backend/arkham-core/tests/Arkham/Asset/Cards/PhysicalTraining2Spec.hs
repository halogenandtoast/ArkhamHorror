module Arkham.Asset.Cards.PhysicalTraining2Spec (
  spec,
) where

import TestImport

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Attrs (InvestigatorAttrs(..))

spec :: Spec
spec = describe "Physical Training (2)" $ do
  it "Adds 1 to willpower check for each resource spent" $ do
    investigator <- testInvestigator $ \attrs ->
      attrs {investigatorWillpower = 1, investigatorResources = 2}
    physicalTraining2 <- buildAsset Assets.physicalTraining2 (Just investigator)

    (didPassTest, logger) <- didPassSkillTestBy investigator SkillWillpower 0

    gameTestWithLogger
      logger
      investigator
      [ SetTokens [Zero]
      , playAsset investigator physicalTraining2
      , beginSkillTest investigator SkillWillpower 3
      ]
      (entitiesL . assetsL %~ insertEntity physicalTraining2)
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

  it "Adds 1 to combat check for each resource spent" $ do
    investigator <- testInvestigator $
      \attrs -> attrs {investigatorCombat = 1, investigatorResources = 2}
    physicalTraining2 <- buildAsset Assets.physicalTraining2 (Just investigator)

    (didPassTest, logger) <- didPassSkillTestBy investigator SkillCombat 0

    gameTestWithLogger
      logger
      investigator
      [ SetTokens [Zero]
      , playAsset investigator physicalTraining2
      , beginSkillTest investigator SkillCombat 3
      ]
      (entitiesL . assetsL %~ insertEntity physicalTraining2)
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

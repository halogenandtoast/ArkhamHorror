module Arkham.Asset.Cards.HardKnocksSpec (
  spec,
) where

import TestImport

import Arkham.Ability
import Arkham.Investigator.Attrs (InvestigatorAttrs(..))

spec :: Spec
spec = describe "Hard Knocks" $ do
  it "Adds 1 to combat check for each resource spent" $ do
    investigator <- testInvestigator $
      \attrs -> attrs {investigatorCombat = 1, investigatorResources = 2}
    hardKnocks <- buildAsset "01049" (Just investigator)

    (didPassTest, logger) <- didPassSkillTestBy investigator SkillCombat 0

    gameTestWithLogger
      logger
      investigator
      [ SetTokens [Zero]
      , playAsset investigator hardKnocks
      , beginSkillTest investigator SkillCombat 3
      ]
      (entitiesL . assetsL %~ insertEntity hardKnocks)
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
    hardKnocks <- buildAsset "01049" (Just investigator)

    (didPassTest, logger) <- didPassSkillTestBy investigator SkillAgility 0

    gameTestWithLogger
      logger
      investigator
      [ SetTokens [Zero]
      , playAsset investigator hardKnocks
      , beginSkillTest investigator SkillAgility 3
      ]
      (entitiesL . assetsL %~ insertEntity hardKnocks)
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

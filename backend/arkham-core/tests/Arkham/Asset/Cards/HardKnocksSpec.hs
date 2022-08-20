module Arkham.Asset.Cards.HardKnocksSpec
  ( spec
  ) where

import TestImport

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Types ( InvestigatorAttrs (..) )

spec :: Spec
spec = describe "Hard Knocks" $ do
  it "Adds 1 to combat check for each resource spent" $ do
    investigator <- testJenny
      $ \attrs -> attrs { investigatorCombat = 1, investigatorResources = 2 }
    hardKnocks <- buildAsset Assets.hardKnocks (Just investigator)

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
            (\case
              AbilityLabel { ability } -> abilityIndex ability == 1
              _ -> False
            )
          chooseOptionMatching
            "use ability"
            (\case
              AbilityLabel { ability } -> abilityIndex ability == 1
              _ -> False
            )
          chooseOptionMatching
            "start skill test"
            (\case
              StartSkillTestButton{} -> True
              _ -> False
            )
          chooseOnlyOption "apply results"
          didPassTest `refShouldBe` True

  it "Adds 1 to agility check for each resource spent" $ do
    investigator <- testJenny
      $ \attrs -> attrs { investigatorAgility = 1, investigatorResources = 2 }
    hardKnocks <- buildAsset Assets.hardKnocks (Just investigator)

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
            (\case
              AbilityLabel { ability } -> abilityIndex ability == 2
              _ -> False
            )
          chooseOptionMatching
            "use ability"
            (\case
              AbilityLabel { ability } -> abilityIndex ability == 2
              _ -> False
            )
          chooseOptionMatching
            "start skill test"
            (\case
              StartSkillTestButton{} -> True
              _ -> False
            )
          chooseOnlyOption "apply results"
          didPassTest `refShouldBe` True

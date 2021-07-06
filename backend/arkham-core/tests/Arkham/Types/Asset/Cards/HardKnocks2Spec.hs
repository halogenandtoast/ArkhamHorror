module Arkham.Types.Asset.Cards.HardKnocks2Spec
  ( spec
  )
where

import TestImport

import Arkham.Types.Investigator.Attrs (InvestigatorAttrs(..))

spec :: Spec
spec = describe "Hard Knocks (2)" $ do
  it "Adds 1 to combat check for each resource spent" $ do
    hardKnocks2 <- buildAsset "50005"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorCombat = 1, investigatorResources = 2 }
    (didPassTest, logger) <- didPassSkillTestBy investigator SkillCombat 0
    gameTestWithLogger
        logger
        investigator
        [ SetTokens [Zero]
        , playAsset investigator hardKnocks2
        , beginSkillTest investigator SkillCombat 3
        ]
        (assetsL %~ insertEntity hardKnocks2)
      $ do
          runMessages
          chooseOptionMatching
            "use ability"
            (\case
              Run{} -> True
              _ -> False
            )
          chooseOptionMatching
            "use ability"
            (\case
              Run{} -> True
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

  it "Adds 1 to agility check for each resource spent" $ do
    hardKnocks2 <- buildAsset "50005"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorAgility = 1, investigatorResources = 2 }

    (didPassTest, logger) <- didPassSkillTestBy investigator SkillAgility 0

    gameTestWithLogger
        logger
        investigator
        [ SetTokens [Zero]
        , playAsset investigator hardKnocks2
        , beginSkillTest investigator SkillAgility 3
        ]
        (assetsL %~ insertEntity hardKnocks2)
      $ do
          runMessages
          chooseOptionMatching
            "use ability"
            (\case
              Run{} -> True
              _ -> False
            )
          chooseOptionMatching
            "use ability"
            (\case
              Run{} -> True
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

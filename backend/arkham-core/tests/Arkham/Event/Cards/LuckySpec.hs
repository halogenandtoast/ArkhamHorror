module Arkham.Event.Cards.LuckySpec
  ( spec
  ) where

import TestImport

import Arkham.Event.Cards qualified as Cards
import Arkham.Investigator.Attrs (InvestigatorAttrs(..))

spec :: Spec
spec = describe "Lucky!" $ do
  it "adds 2 to a skill test when you would fail" $ do
    investigator <- testInvestigator $ \attrs ->
      attrs { investigatorIntellect = 1, investigatorResources = 1 }
    lucky <- genPlayerCard Cards.lucky

    (didPassTest, logger) <- didPassSkillTestBy investigator SkillIntellect 0

    gameTestWithLogger
        logger
        investigator
        [ SetTokens [MinusOne]
        , addToHand investigator (PlayerCard lucky)
        , beginSkillTest investigator SkillIntellect 2
        ]
        id
      $ do
          runMessages
          chooseOnlyOption "start skill test"
          chooseOptionMatching
            "play lucky!"
            (\case
              TargetLabel{} -> True
              _ -> False
            )
          chooseOnlyOption "apply results"
          didPassTest `refShouldBe` True

  it "does not cause an autofail to pass" $ do
    investigator <- testInvestigator $ \attrs ->
      attrs { investigatorIntellect = 1, investigatorResources = 1 }
    lucky <- genPlayerCard Cards.lucky

    (didFailTest, logger) <- didFailSkillTestBy investigator SkillIntellect 2

    gameTestWithLogger
        logger
        investigator
        [ SetTokens [AutoFail]
        , addToHand investigator (PlayerCard lucky)
        , beginSkillTest investigator SkillIntellect 2
        ]
        id
      $ do
          runMessages
          chooseOnlyOption "start skill test"
          chooseOptionMatching
            "play lucky!"
            (\case
              TargetLabel{} -> True
              _ -> False
            )
          chooseOnlyOption "apply results"
          didFailTest `refShouldBe` True

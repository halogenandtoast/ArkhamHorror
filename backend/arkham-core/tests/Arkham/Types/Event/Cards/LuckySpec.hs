module Arkham.Types.Event.Cards.LuckySpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Investigator.Attrs (InvestigatorAttrs(..))

spec :: Spec
spec = describe "Lucky!" $ do
  it "adds 2 to a skill test when you would fail" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorIntellect = 1, investigatorResources = 1 }
    lucky <- buildPlayerCard "01080"
    runGameTest
        investigator
        [ SetTokens [MinusOne]
        , addToHand investigator (PlayerCard lucky)
        , beginSkillTest investigator SkillIntellect 2
        ]
        id
      $ do
          (didPassTest, logger) <- didPassSkillTestBy
            investigator
            SkillIntellect
            0
          runMessagesNoLogging
          runGameTestOnlyOption "start skill test"
          runGameTestOptionMatching
            "play lucky!"
            (\case
              Run{} -> True
              _ -> False
            )
          runGameTestOnlyOptionWithLogger "apply results" logger
          didPassTest `refShouldBe` True

  it "does not cause an autofail to pass" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorIntellect = 1, investigatorResources = 1 }
    lucky <- buildPlayerCard "01080"
    runGameTest
        investigator
        [ SetTokens [AutoFail]
        , addToHand investigator (PlayerCard lucky)
        , beginSkillTest investigator SkillIntellect 2
        ]
        id
      $ do
          (didFailTest, logger) <- didFailSkillTestBy
            investigator
            SkillIntellect
            2
          runMessagesNoLogging
          runGameTestOnlyOption "start skill test"
          runGameTestOptionMatching
            "play lucky!"
            (\case
              Run{} -> True
              _ -> False
            )
          runGameTestOnlyOptionWithLogger "apply results" logger
          didFailTest `refShouldBe` True

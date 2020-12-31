module Arkham.Types.Event.Cards.LuckySpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Investigator.Attrs (Attrs(..))

spec :: Spec
spec = describe "Lucky!" $ do
  it "adds 2 to a skill test when you would fail" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorIntellect = 1, investigatorResources = 1 }
    lucky <- buildPlayerCard "01080"
    (didPassTest, logger) <- didPassSkillTestBy investigator 0
    void
      $ runGameTest
          investigator
          [ SetTokens [MinusOne]
          , addToHand investigator (PlayerCard lucky)
          , beginSkillTest investigator SkillIntellect 2
          ]
          id
      >>= runGameTestOnlyOption "start skill test"
      >>= runGameTestOptionMatching
            "play lucky!"
            (\case
              Run{} -> True
              _ -> False
            )
      >>= runGameTestOnlyOptionWithLogger "apply results" logger
    readIORef didPassTest `shouldReturn` True
  it "does not cause an autofail to pass" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorIntellect = 1, investigatorResources = 1 }
    lucky <- buildPlayerCard "01080"
    (didFailTest, logger) <- didFailSkillTestBy investigator 2
    void
      $ runGameTest
          investigator
          [ SetTokens [AutoFail]
          , addToHand investigator (PlayerCard lucky)
          , beginSkillTest investigator SkillIntellect 2
          ]
          id
      >>= runGameTestOnlyOption "start skill test"
      >>= runGameTestOptionMatching
            "play lucky!"
            (\case
              Run{} -> True
              _ -> False
            )
      >>= runGameTestOnlyOptionWithLogger "apply results" logger
    readIORef didFailTest `shouldReturn` True

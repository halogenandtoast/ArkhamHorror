module Arkham.Types.Event.Cards.LuckySpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Investigator.Attrs (Attrs(..))
import Arkham.Types.Token

spec :: Spec
spec = describe "Lucky!" $ do
  it "adds 2 to a skill test when you would fail" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorIntellect = 1, investigatorResources = 1 }
    lucky <- buildPlayerCard "01080"
    scenario' <- testScenario "00000" id
    game <-
      runGameTest
        investigator
        [ SetTokens [Zero]
        , addToHand investigator (PlayerCard lucky)
        , beginSkillTest investigator SkillIntellect 3
        ]
        (scenario ?~ scenario')
      >>= runGameTestOnlyOption "start skill test"
      >>= runGameTestOptionMatching
            "play lucky!"
            (\case
              Run{} -> True
              _ -> False
            )
      >>= runGameTestOnlyOption "apply results"
    game `shouldSatisfy` hasProcessedMessage
      (PassedSkillTest
        (getId () investigator)
        Nothing
        TestSource
        SkillTestInitiatorTarget
        0
      )

  it "does not cause an autofail to pass" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorIntellect = 1, investigatorResources = 1 }
    lucky <- buildPlayerCard "01080"
    scenario' <- testScenario "00000" id
    game <-
      runGameTest
        investigator
        [ SetTokens [AutoFail]
        , addToHand investigator (PlayerCard lucky)
        , beginSkillTest investigator SkillIntellect 2
        ]
        (scenario ?~ scenario')
      >>= runGameTestOnlyOption "start skill test"
      >>= runGameTestOptionMatching
            "play lucky!"
            (\case
              Run{} -> True
              _ -> False
            )
      >>= runGameTestOnlyOption "apply results"
    game `shouldSatisfy` hasProcessedMessage
      (FailedSkillTest
        (getId () investigator)
        Nothing
        TestSource
        SkillTestInitiatorTarget
        2
      )

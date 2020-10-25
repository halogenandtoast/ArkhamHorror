module Arkham.Types.Event.Cards.WillToSurvive4Spec
  ( spec
  )
where

import TestImport

import Arkham.Types.Investigator.Attrs (Attrs(..))

spec :: Spec
spec = describe "Will to Survive (4)" $ do
  it "cancels all tokens for the turn" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorIntellect = 3 }
    willToSurvive4 <- buildEvent "01085" investigator
    scenario' <- testScenario "00000" id
    game <-
      runGameTest
        investigator
        [ SetTokens [AutoFail]
        , playEvent investigator willToSurvive4
        , beginSkillTest investigator SkillIntellect 3
        ]
        ((scenario ?~ scenario') . (events %~ insertEntity willToSurvive4))
      >>= runGameTestOnlyOption "start skill test"
      >>= runGameTestOnlyOption "apply results"
    game `shouldSatisfy` hasProcessedMessage
      (PassedSkillTest
        (getId () investigator)
        Nothing
        TestSource
        SkillTestInitiatorTarget
        0
      )
  it "it is cancelled at the end of the turn" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorIntellect = 3 }
    willToSurvive4 <- buildEvent "01085" investigator
    scenario' <- testScenario "00000" id
    game <-
      runGameTest
        investigator
        [ SetTokens [AutoFail]
        , playEvent investigator willToSurvive4
        , ChooseEndTurn (getId () investigator)
        , beginSkillTest investigator SkillIntellect 3
        ]
        ((scenario ?~ scenario') . (events %~ insertEntity willToSurvive4))
      >>= runGameTestOnlyOption "start skill test"
      >>= runGameTestOnlyOption "apply results"
    game `shouldSatisfy` hasProcessedMessage
      (FailedSkillTest
        (getId () investigator)
        Nothing
        TestSource
        SkillTestInitiatorTarget
        3
      )

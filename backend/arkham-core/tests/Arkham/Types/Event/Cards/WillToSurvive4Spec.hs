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
    (didPassTest, logger) <- createMessageMatcher
      (PassedSkillTest
        (getInvestigatorId investigator)
        Nothing
        TestSource
        (SkillTestInitiatorTarget TestTarget)
        0
      )
    void
      $ runGameTest
          investigator
          [ SetTokens [AutoFail]
          , playEvent investigator willToSurvive4
          , beginSkillTest investigator SkillIntellect 3
          ]
          (events %~ insertEntity willToSurvive4)
      >>= runGameTestOnlyOption "start skill test"
      >>= runGameTestOnlyOptionWithLogger "apply results" logger
    readIORef didPassTest `shouldReturn` True
  it "it is cancelled at the end of the turn" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorIntellect = 3 }
    willToSurvive4 <- buildEvent "01085" investigator
    (didFailTest, logger) <- createMessageMatcher
      (FailedSkillTest
        (getInvestigatorId investigator)
        Nothing
        TestSource
        (SkillTestInitiatorTarget TestTarget)
        3
      )
    void
      $ runGameTest
          investigator
          [ SetTokens [AutoFail]
          , playEvent investigator willToSurvive4
          , ChooseEndTurn (getInvestigatorId investigator)
          , beginSkillTest investigator SkillIntellect 3
          ]
          (events %~ insertEntity willToSurvive4)
      >>= runGameTestOnlyOption "start skill test"
      >>= runGameTestOnlyOptionWithLogger "apply results" logger
    readIORef didFailTest `shouldReturn` True

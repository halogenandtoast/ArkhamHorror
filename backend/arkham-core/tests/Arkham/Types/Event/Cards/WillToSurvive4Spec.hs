module Arkham.Types.Event.Cards.WillToSurvive4Spec
  ( spec
  )
where

import TestImport

import Arkham.Types.Investigator.Attrs (InvestigatorAttrs(..))

spec :: Spec
spec = describe "Will to Survive (4)" $ do
  it "cancels all tokens for the turn" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorIntellect = 3 }
    willToSurvive4 <- buildEvent "01085" investigator
    runGameTest
        investigator
        [ SetTokens [AutoFail]
        , playEvent investigator willToSurvive4
        , beginSkillTest investigator SkillIntellect 3
        ]
        (eventsL %~ insertEntity willToSurvive4)
      $ do
          (didPassTest, logger) <- didPassSkillTestBy
            investigator
            SkillIntellect
            0
          runMessagesNoLogging
          runGameTestOnlyOption "start skill test"
          runGameTestOnlyOptionWithLogger "apply results" logger
          didPassTest `refShouldBe` True

  it "it is cancelled at the end of the turn" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorIntellect = 3 }
    willToSurvive4 <- buildEvent "01085" investigator
    runGameTest
        investigator
        [ SetTokens [AutoFail]
        , playEvent investigator willToSurvive4
        , ChooseEndTurn (toId investigator)
        , beginSkillTest investigator SkillIntellect 3
        ]
        (eventsL %~ insertEntity willToSurvive4)
      $ do
          (didFailTest, logger) <- didFailSkillTestBy
            investigator
            SkillIntellect
            3
          runMessagesNoLogging
          runGameTestOnlyOption "start skill test"
          runGameTestOnlyOptionWithLogger "apply results" logger
          didFailTest `refShouldBe` True

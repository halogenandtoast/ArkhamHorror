module Arkham.Event.Cards.WillToSurvive4Spec
  ( spec
  ) where

import TestImport

import Arkham.Investigator.Attrs (InvestigatorAttrs(..))

spec :: Spec
spec = describe "Will to Survive (4)" $ do
  it "cancels all tokens for the turn" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorIntellect = 3 }
    willToSurvive4 <- buildEvent "01085" investigator

    (didPassTest, logger) <- didPassSkillTestBy investigator SkillIntellect 0

    gameTestWithLogger
        logger
        investigator
        [ SetTokens [AutoFail]
        , playEvent investigator willToSurvive4
        , beginSkillTest investigator SkillIntellect 3
        ]
        (eventsL %~ insertEntity willToSurvive4)
      $ do
          runMessages
          chooseOnlyOption "start skill test"
          chooseOnlyOption "apply results"
          didPassTest `refShouldBe` True

  it "it is cancelled at the end of the turn" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorIntellect = 3 }
    willToSurvive4 <- buildEvent "01085" investigator

    (didFailTest, logger) <- didFailSkillTestBy investigator SkillIntellect 3

    gameTestWithLogger
        logger
        investigator
        [ SetTokens [AutoFail]
        , playEvent investigator willToSurvive4
        , ChooseEndTurn (toId investigator)
        , beginSkillTest investigator SkillIntellect 3
        ]
        (eventsL %~ insertEntity willToSurvive4)
      $ do
          runMessages
          chooseOnlyOption "start skill test"
          chooseOnlyOption "apply results"
          didFailTest `refShouldBe` True

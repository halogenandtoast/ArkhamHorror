module Arkham.Event.Cards.WillToSurvive3Spec
  ( spec
  ) where

import TestImport

import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Types (InvestigatorAttrs(..))

spec :: Spec
spec = describe "Will to Survive (3)" $ do
  it "cancels all tokens for the turn" $ gameTest $ \investigator -> do
    updateInvestigator investigator
      $ \attrs -> attrs { investigatorIntellect = 3 }

    didPassTest <- didPassSkillTestBy investigator SkillIntellect 0

    pushAndRun $ SetTokens [AutoFail]
    putCardIntoPlay investigator Events.willToSurvive3
    pushAndRun $ beginSkillTest investigator SkillIntellect 3
    chooseOnlyOption "start skill test"
    chooseOnlyOption "apply results"
    didPassTest `refShouldBe` True

  it "it is cancelled at the end of the turn" $ gameTest $ \investigator -> do
    updateInvestigator investigator
      $ \attrs -> attrs { investigatorIntellect = 3 }

    didFailTest <- didFailSkillTestBy investigator SkillIntellect 3

    pushAndRun $ SetTokens [AutoFail]
    putCardIntoPlay investigator Events.willToSurvive3
    pushAndRunAll
      [ ChooseEndTurn (toId investigator)
      , beginSkillTest investigator SkillIntellect 3
      ]
    chooseOnlyOption "start skill test"
    chooseOnlyOption "apply results"
    didFailTest `refShouldBe` True

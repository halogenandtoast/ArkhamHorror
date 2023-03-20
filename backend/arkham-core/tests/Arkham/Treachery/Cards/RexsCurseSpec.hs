module Arkham.Treachery.Cards.RexsCurseSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Investigator.Types
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.SkillTest.Base

spec :: Spec
spec = describe "Rex's Curse" $ do
  it "is put into play into your threat area" $ do
    investigator <- testJenny id
    rexsCurse <- genPlayerCard Cards.rexsCurse
    drawing <- drawCards (toId investigator) investigator 1
    gameTest
        investigator
        [loadDeck investigator [rexsCurse], drawing]
        id
      $ do
          runMessages
          selectAny
              (TreacheryInThreatAreaOf (InvestigatorWithId $ toId investigator)
              <> treacheryIs Cards.rexsCurse
              )
            `shouldReturn` True

  it "causes you to reveal another token" $ do
    investigator <- testJenny (intellectL .~ 5)
    rexsCurse <- genPlayerCard Cards.rexsCurse
    drawing <- drawCards (toId investigator) investigator 1

    (didRunMessage, logger) <- didPassSkillTestBy investigator SkillIntellect 1

    gameTestWithLogger
        logger
        investigator
        [ SetTokens [PlusOne]
        , loadDeck investigator [rexsCurse]
        , drawing
        , BeginSkillTest $ initSkillTest
          (toId investigator)
          (TestSource mempty)
          TestTarget
          SkillIntellect
          5
        ]
        id
      $ do
          runMessages
          chooseOnlyOption "start skill test"
          chooseOnlyOption "trigger rex's curse"
          chooseOnlyOption "apply results"
          selectAny
              (TreacheryInThreatAreaOf (InvestigatorWithId $ toId investigator)
              <> treacheryIs Cards.rexsCurse
              )
            `shouldReturn` True
          didRunMessage `refShouldBe` True

  it "is shuffled back into your deck if you fail the test" $ do
    investigator <- testJenny (intellectL .~ 5)
    rexsCurse <- genPlayerCard Cards.rexsCurse
    drawing <- drawCards (toId investigator) investigator 1
    gameTest
        investigator
        [ SetTokens [MinusOne]
        , loadDeck investigator [rexsCurse]
        , drawing
        , beginSkillTest investigator SkillIntellect 4
        ]
        id
      $ do
          runMessages
          chooseOnlyOption "start skill test"
          -- we sneak in this modifier to cause the next test (with the same token) to fail instead
          push $ skillTestModifier (TestSource mempty) (toTarget investigator) (SkillModifier SkillIntellect (-1))
          runMessages
          chooseOnlyOption "trigger rex's curse"
          chooseOnlyOption "apply results"
          selectAny
              (TreacheryInThreatAreaOf (InvestigatorWithId $ toId investigator)
              <> treacheryIs Cards.rexsCurse
              )
            `shouldReturn` False
          fieldAssert InvestigatorDeck ((== 1) . length) investigator

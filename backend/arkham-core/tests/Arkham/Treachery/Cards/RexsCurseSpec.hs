module Arkham.Treachery.Cards.RexsCurseSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Treachery.Cards qualified as Cards

spec :: Spec
spec = describe "Rex's Curse" $ do
  it "is put into play into your threat area" $ do
    investigator <- testInvestigator id
    rexsCurse <- genPlayerCard Cards.rexsCurse
    gameTest
        investigator
        [loadDeck investigator [rexsCurse], drawCards investigator 1]
        id
      $ do
          runMessages
          investigator' <- updated investigator
          hasTreacheryWithMatchingCardCode (PlayerCard rexsCurse) investigator'
            `shouldReturn` True

  it "causes you to reveal another token" $ do
    investigator <- testInvestigator id
    rexsCurse <- genPlayerCard Cards.rexsCurse

    (didRunMessage, logger) <- didPassSkillTestBy investigator SkillIntellect 2

    gameTestWithLogger
        logger
        investigator
        [ SetTokens [PlusOne]
        , loadDeck investigator [rexsCurse]
        , drawCards investigator 1
        , BeginSkillTest
          (toId investigator)
          (TestSource mempty)
          TestTarget
          Nothing
          SkillIntellect
          5
        ]
        id
      $ do
          runMessages
          chooseOnlyOption "start skill test"
          chooseOnlyOption "apply results"
          investigator' <- updated investigator
          hasTreacheryWithMatchingCardCode (PlayerCard rexsCurse) investigator'
            `shouldReturn` True
          didRunMessage `refShouldBe` True

  fit "is shuffled back into your deck if you fail the test" $ do
    -- TODO: need a way to interject a modifier on skill level
    investigator <- testInvestigator id
    rexsCurse <- genPlayerCard Cards.rexsCurse
    gameTest
        investigator
        [ SetTokens [MinusOne]
        , loadDeck investigator [rexsCurse]
        , drawCards investigator 1
        , beginSkillTest investigator SkillIntellect 4
        ]
        id
      $ do
          runMessages
          chooseOnlyOption "start skill test"
          chooseOnlyOption "trigger rex's curse"
          chooseOnlyOption "reveal new token"
          chooseOnlyOption "apply results"
          investigator' <- updated investigator
          hasTreacheryWithMatchingCardCode (PlayerCard rexsCurse) investigator'
            `shouldReturn` False
          updated investigator `shouldSatisfyM` deckMatches ((== 1) . length)

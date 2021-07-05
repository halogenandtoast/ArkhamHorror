module Arkham.Types.Treachery.Cards.RexsCurseSpec
  ( spec
  ) where

import TestImport.Lifted

spec :: Spec
spec = describe "Rex's Curse" $ do
  it "is put into play into your threat area" $ do
    investigator <- testInvestigator "00000" id
    rexsCurse <- buildPlayerCard "02009"
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
    investigator <- testInvestigator "00000" id
    rexsCurse <- buildPlayerCard "02009"

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

  it "is shuffled back into your deck if you fail the test" $ do
    investigator <- testInvestigator "00000" id
    rexsCurse <- buildPlayerCard "02009"
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
          chooseOnlyOption "apply results"
          investigator' <- updated investigator
          hasTreacheryWithMatchingCardCode (PlayerCard rexsCurse) investigator'
            `shouldReturn` False
          updated investigator `shouldSatisfyM` deckMatches ((== 1) . length)

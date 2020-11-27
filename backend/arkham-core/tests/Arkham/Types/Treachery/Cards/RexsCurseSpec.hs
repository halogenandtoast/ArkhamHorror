module Arkham.Types.Treachery.Cards.RexsCurseSpec
  ( spec
  )
where

import TestImport

spec :: Spec
spec = describe "Rex's Curse" $ do
  it "is put into play into your threat area" $ do
    investigator <- testInvestigator "00000" id
    rexsCurse <- buildPlayerCard "02009"
    game <- runGameTest
      investigator
      [loadDeck investigator [rexsCurse], drawCards investigator 1]
      id
    updated game investigator
      `shouldSatisfy` hasTreacheryWithMatchingCardCode
                        game
                        (PlayerCard rexsCurse)
  it "causes you to reveal another token" $ do
    investigator <- testInvestigator "00000" id
    rexsCurse <- buildPlayerCard "02009"

    (didRunMessage, logger) <- createMessageMatcher
      (PassedSkillTest
        (getInvestigatorId investigator)
        Nothing
        TestSource
        (SkillTestInitiatorTarget TestTarget)
        2
      )
    game <-
      runGameTest
        investigator
        [ SetTokens [PlusOne]
        , loadDeck investigator [rexsCurse]
        , drawCards investigator 1
        , BeginSkillTest
          (getInvestigatorId investigator)
          TestSource
          TestTarget
          Nothing
          SkillIntellect
          5
        ]
        id
      >>= runGameTestOnlyOption "start skill test"
      >>= runGameTestOnlyOptionWithLogger "apply results" logger
    updated game investigator
      `shouldSatisfy` hasTreacheryWithMatchingCardCode
                        game
                        (PlayerCard rexsCurse)
    readIORef didRunMessage `shouldReturn` True
  it "is shuffled back into your deck if you fail the test" $ do
    investigator <- testInvestigator "00000" id
    rexsCurse <- buildPlayerCard "02009"
    game <-
      runGameTest
        investigator
        [ SetTokens [MinusOne]
        , loadDeck investigator [rexsCurse]
        , drawCards investigator 1
        , beginSkillTest investigator SkillIntellect 4
        ]
        id
      >>= runGameTestOnlyOption "start skill test"
      >>= runGameTestOnlyOption "apply results"
    updated game investigator
      `shouldSatisfy` not
      . hasTreacheryWithMatchingCardCode game (PlayerCard rexsCurse)
    updated game investigator `shouldSatisfy` deckMatches ((== 1) . length)

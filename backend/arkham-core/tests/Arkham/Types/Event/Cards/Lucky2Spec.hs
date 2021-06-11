module Arkham.Types.Event.Cards.Lucky2Spec
  ( spec
  )
where

import TestImport.Lifted

import Arkham.Types.Investigator.Attrs (InvestigatorAttrs(..))

spec :: Spec
spec = describe "Lucky! (2)" $ do
  it "adds 2 to a skill test when you would fail and draws 1 card" $ do
    cardToDraw <- testPlayerCard id
    investigator <- testInvestigator "00000" $ \attrs -> attrs
      { investigatorIntellect = 1
      , investigatorResources = 1
      , investigatorDeck = Deck [cardToDraw]
      }
    lucky2 <- buildPlayerCard "01084"
    runGameTest
        investigator
        [ SetTokens [Zero]
        , addToHand investigator (PlayerCard lucky2)
        , beginSkillTest investigator SkillIntellect 3
        ]
        id
      $ do
          (didPassTest, logger) <- didPassSkillTestBy
            investigator
            SkillIntellect
            0
          runMessagesNoLogging
          runGameTestOnlyOption "start skill test"
          runGameTestOptionMatching
            "play lucky!"
            (\case
              Run{} -> True
              _ -> False
            )
          runGameTestOnlyOptionWithLogger "apply results" logger
          didPassTest `refShouldBe` True
          updated investigator `shouldSatisfyM` handIs [PlayerCard cardToDraw]

  it "does not cause an autofail to pass" $ do
    cardToDraw <- testPlayerCard id
    investigator <- testInvestigator "00000" $ \attrs -> attrs
      { investigatorIntellect = 1
      , investigatorResources = 1
      , investigatorDeck = Deck [cardToDraw]
      }
    lucky <- buildPlayerCard "01084"
    runGameTest
        investigator
        [ SetTokens [AutoFail]
        , addToHand investigator (PlayerCard lucky)
        , beginSkillTest investigator SkillIntellect 2
        ]
        id
      $ do
          (didFailTest, logger) <- didFailSkillTestBy
            investigator
            SkillIntellect
            2
          runMessagesNoLogging
          runGameTestOnlyOption "start skill test"
          runGameTestOptionMatching
            "play lucky!"
            (\case
              Run{} -> True
              _ -> False
            )
          runGameTestOnlyOptionWithLogger "apply results" logger
          didFailTest `refShouldBe` True
          updated investigator `shouldSatisfyM` handIs [PlayerCard cardToDraw]

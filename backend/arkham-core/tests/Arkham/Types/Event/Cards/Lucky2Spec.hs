module Arkham.Types.Event.Cards.Lucky2Spec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Event.Cards qualified as Cards
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
    lucky2 <- genPlayerCard Cards.lucky2

    (didPassTest, logger) <- didPassSkillTestBy investigator SkillIntellect 0

    gameTestWithLogger
        logger
        investigator
        [ SetTokens [Zero]
        , addToHand investigator (PlayerCard lucky2)
        , beginSkillTest investigator SkillIntellect 3
        ]
        id
      $ do
          runMessages
          chooseOnlyOption "start skill test"
          chooseOptionMatching
            "play lucky!"
            (\case
              Run{} -> True
              _ -> False
            )
          chooseOnlyOption "apply results"
          didPassTest `refShouldBe` True
          updated investigator `shouldSatisfyM` handIs [PlayerCard cardToDraw]

  it "does not cause an autofail to pass" $ do
    cardToDraw <- testPlayerCard id
    investigator <- testInvestigator "00000" $ \attrs -> attrs
      { investigatorIntellect = 1
      , investigatorResources = 1
      , investigatorDeck = Deck [cardToDraw]
      }
    lucky2 <- genPlayerCard Cards.lucky2

    (didFailTest, logger) <- didFailSkillTestBy investigator SkillIntellect 2

    gameTestWithLogger
        logger
        investigator
        [ SetTokens [AutoFail]
        , addToHand investigator (PlayerCard lucky2)
        , beginSkillTest investigator SkillIntellect 2
        ]
        id
      $ do
          runMessages
          chooseOnlyOption "start skill test"
          chooseOptionMatching
            "play lucky!"
            (\case
              Run{} -> True
              _ -> False
            )
          chooseOnlyOption "apply results"
          didFailTest `refShouldBe` True
          updated investigator `shouldSatisfyM` handIs [PlayerCard cardToDraw]

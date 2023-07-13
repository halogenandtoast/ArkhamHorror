module Arkham.Event.Cards.Lucky2Spec (
  spec,
) where

import TestImport.Lifted

import Arkham.Event.Cards qualified as Cards
import Arkham.Investigator.Types (InvestigatorAttrs (..))

spec :: Spec
spec = describe "Lucky! (2)" $ do
  it "adds 2 to a skill test when you would fail and draws 1 card" $ gameTest $ \investigator -> do
    cardToDraw <- testPlayerCard id
    updateInvestigator investigator $ \attrs ->
      attrs
        { investigatorIntellect = 1
        , investigatorResources = 1
        , investigatorDeck = Deck [cardToDraw]
        }
    lucky2 <- genCard Cards.lucky2

    didPassTest <- didPassSkillTestBy investigator SkillIntellect 0

    pushAndRunAll
      [ SetChaosTokens [Zero]
      , addToHand (toId investigator) lucky2
      , beginSkillTest investigator SkillIntellect 3
      ]

    chooseOnlyOption "start skill test"
    chooseOptionMatching
      "play lucky!"
      ( \case
          TargetLabel {} -> True
          _ -> False
      )
    chooseOnlyOption "apply results"
    didPassTest `refShouldBe` True
    assert $ handIs [PlayerCard cardToDraw] investigator

  it "does not cause an autofail to pass" $ gameTest $ \investigator -> do
    cardToDraw <- testPlayerCard id
    updateInvestigator investigator $ \attrs ->
      attrs
        { investigatorIntellect = 1
        , investigatorResources = 1
        , investigatorDeck = Deck [cardToDraw]
        }
    lucky2 <- genCard Cards.lucky2

    didFailTest <- didFailSkillTestBy investigator SkillIntellect 2

    pushAndRunAll
      [ SetChaosTokens [AutoFail]
      , addToHand (toId investigator) lucky2
      , beginSkillTest investigator SkillIntellect 2
      ]
    chooseOnlyOption "start skill test"
    chooseOptionMatching
      "play lucky!"
      ( \case
          TargetLabel {} -> True
          _ -> False
      )
    chooseOnlyOption "apply results"
    didFailTest `refShouldBe` True
    assert $ handIs [PlayerCard cardToDraw] investigator

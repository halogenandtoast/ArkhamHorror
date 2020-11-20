module Arkham.Types.Event.Cards.Lucky2Spec
  ( spec
  )
where

import TestImport

import Arkham.Types.Investigator.Attrs (Attrs(..))

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
    scenario' <- testScenario "00000" id
    game <-
      runGameTest
        investigator
        [ SetTokens [Zero]
        , addToHand investigator (PlayerCard lucky2)
        , beginSkillTest investigator SkillIntellect 3
        ]
        (scenario ?~ scenario')
      >>= runGameTestOnlyOption "start skill test"
      >>= runGameTestOptionMatching
            "play lucky!"
            (\case
              Run{} -> True
              _ -> False
            )
      >>= runGameTestOnlyOption "apply results"
    game `shouldSatisfy` hasProcessedMessage
      (PassedSkillTest
        (getInvestigatorId investigator)
        Nothing
        TestSource
        (SkillTestInitiatorTarget TestTarget)
        0
      )
    updated game investigator `shouldSatisfy` handIs [PlayerCard cardToDraw]

  it "does not cause an autofail to pass" $ do
    cardToDraw <- testPlayerCard id
    investigator <- testInvestigator "00000" $ \attrs -> attrs
      { investigatorIntellect = 1
      , investigatorResources = 1
      , investigatorDeck = Deck [cardToDraw]
      }
    lucky <- buildPlayerCard "01084"
    scenario' <- testScenario "00000" id
    game <-
      runGameTest
        investigator
        [ SetTokens [AutoFail]
        , addToHand investigator (PlayerCard lucky)
        , beginSkillTest investigator SkillIntellect 2
        ]
        (scenario ?~ scenario')
      >>= runGameTestOnlyOption "start skill test"
      >>= runGameTestOptionMatching
            "play lucky!"
            (\case
              Run{} -> True
              _ -> False
            )
      >>= runGameTestOnlyOption "apply results"
    game `shouldSatisfy` hasProcessedMessage
      (FailedSkillTest
        (getInvestigatorId investigator)
        Nothing
        TestSource
        (SkillTestInitiatorTarget TestTarget)
        2
      )
    updated game investigator `shouldSatisfy` handIs [PlayerCard cardToDraw]

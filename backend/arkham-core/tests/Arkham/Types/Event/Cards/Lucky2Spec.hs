module Arkham.Types.Event.Cards.Lucky2Spec
  ( spec
  )
where

import TestImport

import Arkham.Types.Helpers
import Arkham.Types.Investigator.Attrs (Attrs(..))
import Arkham.Types.Target
import Arkham.Types.Token

spec :: Spec
spec = describe "Lucky! (2)" $ do
  it "adds 2 to a skill test when you would fail and draws 1 card" $ do
    cardToDraw <- testPlayerCard
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
        (getId () investigator)
        Nothing
        TestSource
        SkillTestInitiatorTarget
        0
      )
    updated game investigator `shouldSatisfy` handIs [PlayerCard cardToDraw]

  it "does not cause an autofail to pass" $ do
    cardToDraw <- testPlayerCard
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
        (getId () investigator)
        Nothing
        TestSource
        SkillTestInitiatorTarget
        2
      )
    updated game investigator `shouldSatisfy` handIs [PlayerCard cardToDraw]

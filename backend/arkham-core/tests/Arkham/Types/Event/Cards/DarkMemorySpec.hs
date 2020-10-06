module Arkham.Types.Event.Cards.DarkMemorySpec
  ( spec
  )
where

import TestImport

spec :: Spec
spec = do
  describe "Dark Memory" $ do
    it "places 1 doom and can advance current agenda" $ do
      investigator <- testInvestigator "00000" id
      agenda <- testAgenda "00000" id
      scenario' <- testScenario "00000" id
      darkMemory <- buildEvent "01013" investigator
      game <-
        runGameTest investigator [playEvent investigator darkMemory]
        $ (events %~ insertEntity darkMemory)
        . (agendas %~ insertEntity agenda)
        . (scenario ?~ scenario')
      agenda `shouldSatisfy` hasDoom game 1
      darkMemory `shouldSatisfy` isInDiscardOf game investigator
      game `shouldSatisfy` hasProcessedMessage (AdvanceAgenda "00000")

    it "is revealed and deals 2 horror if in hand at end of turn" $ do
      investigator <- testInvestigator "00000" id
      darkMemory <- buildPlayerCard "01013"
      game <-
        runGameTest
          investigator
          [ addToHand investigator (PlayerCard darkMemory)
          , chooseEndTurn investigator
          ]
          id
        >>= runGameTestOnlyOption "assign first horror"
        >>= runGameTestOnlyOption "assign second horror"
      updated game investigator `shouldSatisfy` hasDamage (0, 2)
      game `shouldSatisfy` hasProcessedMessage
        (RevealInHand $ getCardId darkMemory)

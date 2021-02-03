module Arkham.Types.Event.Cards.DarkMemorySpec
  ( spec
  ) where

import TestImport

spec :: Spec
spec = do
  describe "Dark Memory" $ do
    it "places 1 doom and can advance current agenda" $ do
      investigator <- testInvestigator "00000" id
      agenda <- testAgenda "00000" id
      darkMemory <- buildEvent "01013" investigator
      (didAdvanceAgenda, logger) <- createMessageMatcher (AdvanceAgenda "00000")

      game <-
        runGameTestWithLogger
            logger
            investigator
            [playEvent investigator darkMemory]
            ((eventsL %~ insertEntity darkMemory)
            . (agendasL %~ insertEntity agenda)
            )
          >>= runGameTestOnlyOption "Advance agenda"
      agenda `shouldSatisfy` hasDoom game 1
      darkMemory `shouldSatisfy` isInDiscardOf game investigator
      readIORef didAdvanceAgenda `shouldReturn` True
    it "is revealed and deals 2 horror if in hand at end of turn" $ do
      investigator <- testInvestigator "00000" id
      darkMemory <- buildPlayerCard "01013"
      (didReveal, logger) <- createMessageMatcher
        (RevealInHand $ getCardId darkMemory)
      game <-
        runGameTestWithLogger
          logger
          investigator
          [ addToHand investigator (PlayerCard darkMemory)
          , chooseEndTurn investigator
          ]
          id
        >>= runGameTestOnlyOption "assign first horror"
        >>= runGameTestOnlyOption "assign second horror"
      updated game investigator `shouldSatisfy` hasDamage (0, 2)
      readIORef didReveal `shouldReturn` True

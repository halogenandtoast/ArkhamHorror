module Arkham.Types.Event.Cards.DarkMemorySpec
  ( spec
  )
where

import TestImport.Lifted

spec :: Spec
spec = do
  describe "Dark Memory" $ do
    it "places 1 doom and can advance current agenda" $ do
      investigator <- testInvestigator "00000" id
      agenda <- testAgenda "00000" id
      darkMemory <- buildEvent "01013" investigator

      runGameTest
          investigator
          [playEvent investigator darkMemory]
          ((eventsL %~ insertEntity darkMemory)
          . (agendasL %~ insertEntity agenda)
          )
        $ do
            (didAdvanceAgenda, logger) <- createMessageMatcher
              (AdvanceAgenda "00000")
            void $ runMessages logger
            runGameTestOnlyOption "Advance agenda"
            getDoom agenda `shouldReturn` 1
            isInDiscardOf investigator darkMemory `shouldReturn` True
            didAdvanceAgenda `refShouldBe` True

    it "is revealed and deals 2 horror if in hand at end of turn" $ do
      investigator <- testInvestigator "00000" id
      darkMemory <- buildPlayerCard "01013"
      runGameTest
          investigator
          [ addToHand investigator (PlayerCard darkMemory)
          , chooseEndTurn investigator
          ]
          id
        $ do
            (didReveal, logger) <- createMessageMatcher
              (RevealInHand $ getCardId darkMemory)
            void $ runMessages logger
            runGameTestOnlyOption "assign first horror"
            runGameTestOnlyOption "assign second horror"
            updated investigator `shouldSatisfyM` hasDamage (0, 2)
            didReveal `refShouldBe` True

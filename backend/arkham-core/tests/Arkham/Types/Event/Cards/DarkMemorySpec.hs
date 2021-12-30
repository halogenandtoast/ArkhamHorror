module Arkham.Event.Cards.DarkMemorySpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Event.Cards qualified as Cards
import Arkham.Agenda.Attrs qualified as Agenda

spec :: Spec
spec = do
  describe "Dark Memory" $ do
    it "places 1 doom and can advance current agenda" $ do
      investigator <- testInvestigator "00000" id
      agenda <- testAgenda "00000" (Agenda.doomThresholdL .~ Static 1)
      darkMemory <- buildEvent "01013" investigator

      (didAdvanceAgenda, logger) <- createMessageMatcher (AdvanceAgenda "00000")

      gameTestWithLogger
          logger
          investigator
          [playEvent investigator darkMemory]
          ((eventsL %~ insertEntity darkMemory)
          . (agendasL %~ insertEntity agenda)
          )
        $ do
            void runMessages
            chooseOnlyOption "Advance agenda"
            chooseOptionMatching
              "have to choose horror option to avoid discard"
              (\case
                Label "The lead investigator takes 2 horror" _ -> True
                _ -> False
              )
            chooseFirstOption "assign first horror"
            chooseFirstOption "assign second horror"
            isInDiscardOf investigator darkMemory `shouldReturn` True
            didAdvanceAgenda `refShouldBe` True

    it "is revealed and deals 2 horror if in hand at end of turn" $ do
      investigator <- testInvestigator "00000" id
      darkMemory <- genPlayerCard Cards.darkMemory

      (didReveal, logger) <- createMessageMatcher
        (RevealInHand $ toCardId darkMemory)

      gameTestWithLogger
          logger
          investigator
          [ addToHand investigator (PlayerCard darkMemory)
          , chooseEndTurn investigator
          ]
          id
        $ do
            void runMessages
            chooseOnlyOption "assign first horror"
            chooseOnlyOption "assign second horror"
            updated investigator `shouldSatisfyM` hasDamage (0, 2)
            didReveal `refShouldBe` True

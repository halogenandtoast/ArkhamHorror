module Arkham.Event.Cards.DarkMemorySpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Event.Cards qualified as Events
import Arkham.Agenda.Types qualified as Agenda
import Arkham.Investigator.Types (Field(..))

spec :: Spec
spec = do
  describe "Dark Memory" $ do
    it "places 1 doom and can advance current agenda" $ do
      investigator <- testJenny id
      agenda <- testAgenda "01105" (Agenda.doomThresholdL ?~ Static 1)
      darkMemory <- buildEvent Events.darkMemory investigator

      (didAdvanceAgenda, logger) <- createMessageMatcher (AdvanceAgenda "01105")

      gameTestWithLogger
          logger
          investigator
          [playEvent investigator darkMemory]
          ((entitiesL . eventsL %~ insertEntity darkMemory)
          . (entitiesL . agendasL %~ insertEntity agenda)
          )
        $ do
            runMessages
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
      investigator <- testJenny id
      darkMemory <- genPlayerCard Events.darkMemory

      (didReveal, logger) <- createMessageMatcher
        (RevealInHand $ toCardId darkMemory)

      gameTestWithLogger
          logger
          investigator
          [ addToHand investigator darkMemory
          , chooseEndTurn investigator
          ]
          id
        $ do
            runMessages
            chooseOnlyOption "trigger dark memory"
            chooseOnlyOption "assign first horror"
            chooseOnlyOption "assign second horror"
            fieldAssert InvestigatorHorror (== 2) investigator
            didReveal `refShouldBe` True

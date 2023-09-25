module Arkham.Event.Cards.DarkMemorySpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Agenda.Types qualified as Agenda
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Types (Field (..))

spec :: Spec
spec = do
  describe "Dark Memory" $ do
    it "places 1 doom and can advance current agenda" $ gameTest $ \investigator -> do
      _ <- testAgenda "01105" (Agenda.doomThresholdL ?~ Static 1)

      didAdvanceAgenda <- createMessageMatcher (AdvanceAgenda "01105")

      putCardIntoPlay investigator Events.darkMemory

      chooseOnlyOption "Advance agenda"
      chooseOptionMatching
        "have to choose horror option to avoid discard"
        ( \case
            Label "The lead investigator takes 2 horror" _ -> True
            _ -> False
        )
      chooseFirstOption "assign first horror"
      chooseFirstOption "assign second horror"
      assert $ isInDiscardOf investigator Events.darkMemory
      didAdvanceAgenda `refShouldBe` True

    it "is revealed and deals 2 horror if in hand at end of turn" $ gameTest $ \investigator -> do
      darkMemory <- genPlayerCard Events.darkMemory

      didReveal <- createMessageMatcher (RevealCard $ toCardId darkMemory)

      pushAndRunAll
        [ addToHand (toId investigator) darkMemory
        , chooseEndTurn investigator
        ]
      chooseOnlyOption "trigger dark memory"
      chooseOnlyOption "assign first horror"
      chooseOnlyOption "assign second horror"
      fieldAssert InvestigatorHorror (== 2) investigator
      didReveal `refShouldBe` True

module Arkham.Event.Cards.DarkMemorySpec (spec) where

import Arkham.Agenda.Types qualified as Agenda
import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = do
  describe "Dark Memory" $ do
    it "places 1 doom and can advance current agenda" . gameTest $ \self -> do
      _ <- testAgenda "01105" (Agenda.doomThresholdL ?~ Static 1)
      didAdvanceAgenda <- createMessageMatcher (AdvanceAgenda "01105")
      self `putCardIntoPlay` Events.darkMemory
      click "Advance agenda"
      chooseOptionMatching "have to choose horror option to avoid discard" \case
        Label "The lead investigator takes 2 horror" _ -> True
        _ -> False
      click "assign first horror"
      click "assign second horror"
      assert $ Events.darkMemory `isInDiscardOf` self
      didAdvanceAgenda `refShouldBe` True

    it "is revealed and deals 2 horror if in hand at end of turn" . gameTest $ \self -> do
      darkMemory <- genPlayerCard Events.darkMemory
      didReveal <- createMessageMatcher (RevealCard $ toCardId darkMemory)
      self `addToHand` darkMemory
      run $ chooseEndTurn self
      click "trigger dark memory"
      click "assign first horror"
      click "assign second horror"
      self.horror `shouldReturn` 2
      didReveal `refShouldBe` True

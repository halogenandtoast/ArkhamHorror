module Arkham.Location.Cards.AgesUnwound.ThePresentSpec (spec) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Phase (Phase (InvestigationPhase, MythosPhase))
import TestImport.New

spec :: Spec
spec = describe "The Present" do
  for_ [InvestigationPhase, MythosPhase] \startingPhase ->
    it ("spends this round's action and restores the normal pool next round: " <> show startingPhase)
      . gameTest
      $ \self -> do
        location <- testLocationWithDef Locations.thePresent id
        self `moveTo` location
        withProp @"remainingActions" 3 self
        overTest $ (phaseL .~ startingPhase) . (turnPlayerInvestigatorIdL .~ Nothing)
        run $ UseCardAbility self.id (toSource location) 1 [] NoPayment
        self.remainingActions `shouldReturn` 3
        takeResource self
        self.remainingActions `shouldReturn` 2
        when (startingPhase == MythosPhase) do
          overTest $ phaseL .~ InvestigationPhase
          run $ Again (Begin InvestigationPhase)
          self.remainingActions `shouldReturn` 2
        run $ BeginTurn self.id
        self.remainingActions `shouldReturn` 2
        run EndRound
        run $ Do BeginRound
        self.remainingActions `shouldReturn` 3

module Arkham.Investigator.RunnerSpec (spec) where

import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Token
import TestImport.New

spec :: Spec
spec = describe "Investigator.Runner" do
  it "moves one clue between investigators without duplicating it" . gameTest $ \self -> do
    other <- addInvestigator Investigators.rolandBanks
    run $ PlaceTokens (TestSource mempty) (toTarget self) Clue 1

    run $ MoveTokens (TestSource mempty) (toSource self) (toTarget other) Clue 1

    self.clues `shouldReturn` 0
    other.clues `shouldReturn` 1

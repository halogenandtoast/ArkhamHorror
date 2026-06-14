module Arkham.Treachery.Cards.PsychotropicSporesSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Psychotropic Spores" $ do
  it "deals 1 direct horror on the first card drawn each round" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    _ <- self `putTreacheryIntoPlay` Treacheries.psychotropicSpores
    self `drawsCard` Assets.flashlight
    useForcedAbility
    applyAllHorror
    self.horror `shouldReturn` 1

  it "does not deal horror again on a later draw the same round" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    _ <- self `putTreacheryIntoPlay` Treacheries.psychotropicSpores
    self `drawsCard` Assets.flashlight
    useForcedAbility
    applyAllHorror
    self `drawsCard` Assets.flashlight
    self.horror `shouldReturn` 1

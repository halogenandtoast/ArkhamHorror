module Arkham.Treachery.Cards.PsychosisSpec (spec) where

import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Psychosis" $ do
  context "After you take 1 or more horror" $ do
    it "deals 1 direct damage" . gameTest $ \self -> do
      location <- testLocation
      self `moveTo` location
      self `drawsCard` Treacheries.psychosis
      run $ assignHorror (toId self) (TestSource mempty) 1
      applyAllHorror
      useForcedAbility
      assertDamageIsDirect
      applyAllDamage
      self.horror `shouldReturn` 1
      self.damage `shouldReturn` 1

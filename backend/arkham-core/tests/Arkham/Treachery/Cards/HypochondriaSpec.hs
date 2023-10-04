module Arkham.Treachery.Cards.HypochondriaSpec (spec) where

import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Hypochondria" $ do
  context "After you take 1 or more damage" $ do
    it "deals 1 direct horror" . gameTest $ \self -> do
      location <- testLocation
      self `moveTo` location
      self `drawsCard` Treacheries.hypochondria
      run $ assignDamage (toId self) (TestSource mempty) 1
      applyAllDamage
      useForcedAbility
      assertHorrorIsDirect
      applyAllHorror
      self.horror `shouldReturn` 1
      self.damage `shouldReturn` 1

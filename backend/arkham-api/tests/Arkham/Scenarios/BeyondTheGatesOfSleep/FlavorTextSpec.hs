module Arkham.Scenarios.BeyondTheGatesOfSleep.FlavorTextSpec (spec) where

import Arkham.Prelude
import Arkham.Scenarios.BeyondTheGatesOfSleep.FlavorText
import Arkham.Text
import Test.Hspec

spec :: Spec
spec = describe "Beyond the Gates of Sleep flavor text" do
  it "uses locale keys for the Mystic dream" do
    mysticDream
      `shouldBe` FlavorText
        (Just "$theDreamEaters.beyondTheGatesOfSleep.dreams.mystic.title")
        [I18nEntry "theDreamEaters.beyondTheGatesOfSleep.dreams.mystic.body" mempty]

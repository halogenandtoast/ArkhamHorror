module Arkham.Investigator.Cards.LukeRobinsonSpec (spec) where

import TestImport.New

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards (lukeRobinson)

{-

-}

spec :: Spec
spec = describe "Luke Robinson" do
  beginsWithInPlay lukeRobinson Assets.gateBox

  it
    "You may play one event each turn as if you were at a connecting location and engaged with each enemy at that location."
    . gameTestWith lukeRobinson
    $ \self -> do
      (location1, location2) <- testConnectedLocations id id
      etherealForm <- genCard Events.etherealForm
      withProp @"hand" [etherealForm] self
      withProp @"resources" 2 self
      self `moveTo` location1
      enemy <- testEnemy
      enemy `spawnAt` location2
      duringTurn self do
        self.playableCards `shouldReturn` [etherealForm]

  context "Elder Sign" do
    it "+1" . gameTestWith lukeRobinson $ \self -> do
      self.elderSignModifier `shouldReturn` PositiveModifier 1

    it "places 1 charge on gate box" . gameTestWith lukeRobinson $ \self -> do
      gateBox <- self `putAssetIntoPlay` Assets.gateBox
      setChaosTokens [ElderSign]
      run $ beginSkillTest self #combat 100
      skip
      startSkillTest
      skip
      applyResults
      -- 3 initial plus 1 from elder sign
      gateBox.charges `shouldReturn` 4

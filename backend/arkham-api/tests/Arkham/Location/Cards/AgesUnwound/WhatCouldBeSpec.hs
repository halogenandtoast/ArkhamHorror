module Arkham.Location.Cards.AgesUnwound.WhatCouldBeSpec (spec) where

import Arkham.Ability.Types (abilityIndex)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import TestImport.New

spec :: Spec
spec = describe "What Could Be" do
  for_ [Locations.whatCouldBe_197, Locations.whatCouldBe_198] \def -> do
    it
      ("cannot repeat the connection effect while every location is connected: " <> show (toCardCode def))
      . gameTest
      $ \self -> do
        location <- testLocationWithDef def id
        other <- testLocation
        self `moveTo` other
        run $ BeginTurn self.id
        let offered = map abilityIndex <$> getActionsFrom self location
        offered `shouldSatisfyM` elem 1
        run $ UseCardAbility self.id (toSource location) 1 [] NoPayment
        offered `shouldSatisfyM` notElem 1
        run EndRound
        run $ BeginTurn self.id
        offered `shouldSatisfyM` elem 1

    it ("can connect a new location that is missing the reverse connection: " <> show (toCardCode def))
      . gameTest
      $ \self -> do
        location <- testLocationWithDef def id
        other <- testLocation
        self `moveTo` other
        run $ BeginTurn self.id
        run $ UseCardAbility self.id (toSource location) 1 [] NoPayment
        (map abilityIndex <$> getActionsFrom self location) `shouldSatisfyM` notElem 1
        _ <- testLocation
        run Noop
        (map abilityIndex <$> getActionsFrom self location) `shouldSatisfyM` elem 1

module Arkham.Treachery.Cards.PanicSpec (spec) where

import Arkham.Constants (pattern ResourceAbility)
import Arkham.GameEnv (getWindowTick)
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Treachery.CardDefs.EdgeOfTheEarth qualified as Treacheries
import Arkham.Window qualified as Window
import TestImport.New

spec :: Spec
spec = describe "Panic" do
  it "pins the resource activation before its attacks of opportunity" . gameTest $ \self -> do
    location <- testLocation
    enemy <- testEnemy
    self `moveTo` location
    enemy `spawnAt` location
    run $ BeginTurn self.id
    activationTick <- getWindowTick
    takeResource self
    queued <- peekQueue
    let activationTicks =
          [ Window.windowConditionTick window
          | CheckWindows windows <- queued
          , window <- windows
          , Timing.After <- [Window.windowTiming window]
          , Window.ActivateAbility _ _ ability <- [Window.windowType window]
          , ability.index == ResourceAbility
          ]
    activationTicks `shouldBe` [Just activationTick]

  it "does not trigger for an action it entered play during, but does for the next action" . gameTest $ \self -> do
    location <- testLocation
    enemy <- testEnemy
    self `moveTo` location
    enemy `spawnAt` location
    run $ BeginTurn self.id
    takeResource self
    -- Enter during the pending attack-of-opportunity decision. The original
    -- resource activation has already begun, even though its after-window has not.
    self `drawsCard` Treacheries.panic
    assertHasNoReaction
    self.resources `shouldReturn` 1
    discard enemy
    takeResource self
    useForcedAbility
    assertAny $ treacheryIs Treacheries.panic

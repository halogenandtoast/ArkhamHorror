module Arkham.Event.Events.GuerrillaTactics2Spec (spec) where

import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Guerrilla Tactics (2)" do
  it "gives +2 combat when fighting" . gameTest $ \self -> do
    (here, connecting) <- testConnectedLocations id id
    self `moveTo` here
    enemy <- testEnemy
    enemy `spawnAt` connecting
    guerrillaTactics2 <- genCard Events.guerrillaTactics2
    withProp @"combat" 1 self
    withProp @"resources" 1 self
    self `addToHand` guerrillaTactics2

    duringTurn self do
      self `playCard` guerrillaTactics2
      clickLabel "$cards.label.guerrillaTactics.fight"
      chooseTarget enemy
      self.skillValue `shouldReturn` 3

  it "gives +2 agility when evading" . gameTest $ \self -> do
    (here, connecting) <- testConnectedLocations id id
    self `moveTo` here
    enemy <- testEnemy
    enemy `spawnAt` connecting
    guerrillaTactics2 <- genCard Events.guerrillaTactics2
    withProp @"agility" 1 self
    withProp @"resources" 1 self
    self `addToHand` guerrillaTactics2

    duringTurn self do
      self `playCard` guerrillaTactics2
      clickLabel "$cards.label.guerrillaTactics.evade"
      chooseTarget enemy
      self.skillValue `shouldReturn` 3

module Arkham.Event.Events.PutrescentRotSpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Event.Cards qualified as Events
import Arkham.Matcher (cardIs, eventIs)
import Arkham.Placement
import TestImport.New

spec :: Spec
spec = describe "Putrescent Rot" do
  it "returns to your bonded cards when the attached enemy is discarded" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    zoog <- testEnemyWithDef Enemies.furtiveZoog id
    zoog `spawnAt` location
    rot <- genMyCard self Events.putrescentRot
    run $ CreateEventAt self.id rot (AttachedToEnemy $ toId zoog)

    discard zoog
    click "trigger putrescent rot"
    assert $ selectNone $ eventIs Events.putrescentRot
    self.bonded `shouldReturn` [rot]

  -- #5309: removing the attached enemy from the game is still leaving play, but
  -- the encounter-set purge used to skip the leave-play windows entirely.
  it "returns to your bonded cards when the attached enemy is removed from the game" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    zoog <- testEnemyWithDef Enemies.furtiveZoog id
    zoog `spawnAt` location
    rot <- genMyCard self Events.putrescentRot
    run $ CreateEventAt self.id rot (AttachedToEnemy $ toId zoog)

    run $ RemoveAllCopiesOfEncounterCardFromGame (cardIs Enemies.furtiveZoog)
    click "trigger putrescent rot"
    assert $ selectNone $ eventIs Events.putrescentRot
    self.bonded `shouldReturn` [rot]

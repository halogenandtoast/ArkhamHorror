module Arkham.Enemy.Cards.HordeOfNightSpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher qualified as Matcher
import TestImport.New

spec :: Spec
spec = describe "Horde of Night" do
  -- Regression for issue #5314. Horde of Night is printed "Swarming 1 (per
  -- investigator)" but was defined with a static value, so it always spawned a
  -- single swarm card no matter how many players were in the game.
  it "spawns one swarm card per investigator" . gameTest $ \self -> do
    other <- addInvestigator Investigators.rolandBanks
    location <- testLocation
    self `moveTo` location
    other `moveTo` location

    -- Swarm cards are dealt face down off the lead investigator's deck, so it
    -- needs enough cards to cover the swarm
    loadDeckCards self =<< testPlayerCards 5

    hordeOfNight <- testEnemyWithDef Enemies.hordeOfNight id
    hordeOfNight `spawnAt` location

    selectCount (Matcher.SwarmOf $ toId hordeOfNight) `shouldReturn` 2

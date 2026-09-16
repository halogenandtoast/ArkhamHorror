module Arkham.Enemy.Cards.TheDreamEaters.TheSearchForKadath.HordeOfNightSpec (spec) where

import Arkham.DamageEffect (nonAttack)
import Arkham.Enemy.CardDefs.TheDreamEaters.TheSearchForKadath qualified as Enemies
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

  -- "Horde of Night's host card cannot be defeated. If it would be defeated,
  -- exhaust Horde of Night, instead." The two halves of that interact with the
  -- swarm rules: a host enemy cannot be defeated while it still has swarm cards,
  -- so while any remain there is no would-be-defeat for the exhaust to replace.
  -- Zulan-Thek hands the host a fresh swarm card at the end of every round, which
  -- is what puts it back in that state between exhausts.
  it "does not exhaust while it still has swarm cards" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    loadDeckCards self =<< testPlayerCards 5

    hordeOfNight <- testEnemyWithDef Enemies.hordeOfNight id
    hordeOfNight `spawnAt` location
    assert $ selectAny (Matcher.SwarmOf $ toId hordeOfNight)

    run $ DealDamage (toTarget hordeOfNight) (nonAttack (Just self.id) (TestSource mempty) 1)
    applyAllDamage

    -- The damage lands and stays on the host, but it is neither defeated nor
    -- exhausted while the swarm is still there
    hordeOfNight.damage `shouldReturn` 1
    hordeOfNight.exhausted `shouldReturn` False
    assert $ selectAny (Matcher.EnemyWithId $ toId hordeOfNight)

  -- Damage is never healed off the host, so once its swarm is gone every further
  -- point of damage is another would-be-defeat, and each one exhausts it again.
  it "exhausts again each time it would be defeated once its swarm is gone" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    loadDeckCards self =<< testPlayerCards 5

    hordeOfNight <- testEnemyWithDef Enemies.hordeOfNight id
    hordeOfNight `spawnAt` location

    swarm <- select (Matcher.SwarmOf $ toId hordeOfNight)
    for_ swarm $ run . RemoveEnemy

    let damageHost =
          run $ DealDamage (toTarget hordeOfNight) (nonAttack (Just self.id) (TestSource mempty) 1)

    damageHost
    applyAllDamage
    hordeOfNight.damage `shouldReturn` 1
    hordeOfNight.exhausted `shouldReturn` True

    run $ Ready (toTarget hordeOfNight)

    damageHost
    applyAllDamage
    hordeOfNight.damage `shouldReturn` 2
    hordeOfNight.exhausted `shouldReturn` True

    -- Still never actually defeated
    assert $ selectAny (Matcher.EnemyWithId $ toId hordeOfNight)

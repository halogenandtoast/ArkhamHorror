module Arkham.Enemy.EngagementSpec (spec) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher qualified as Matcher
import TestImport.New

-- Spawns a swarming host engaged with `self` and returns the host and its one
-- swarm card. `testEnemy` is inserted directly into the entity map, so the
-- `Swarming` spawn flow never runs and we place the swarm card by hand.
swarmingEnemyEngagedWith :: Investigator -> Location -> TestAppT (EnemyId, EnemyId)
swarmingEnemyEngagedWith self location = do
  host <- testEnemy
  host `spawnAt` location
  run $ EnemyEngageInvestigator (toId host) (toId self)
  card <- genCard Cards.swarmOfRats
  run $ PlacedSwarmCard (toId host) card
  swarm <- selectJust $ Matcher.SwarmOf (toId host)
  pure (toId host, swarm)

spec :: Spec
spec = describe "Enemy engagement" do
  -- Regression for issue #5292 (Redeem a Former Colleague moving an engaged
  -- Edwin Bennet). `EnemyEntered` used to preserve an `InThreatArea` placement
  -- and let `Do (EnemyMove)` rewrite it, but that write runs *after* the
  -- after-enters windows — so `EnemyCheckEngagement` saw a still-engaged enemy,
  -- bailed, and the enemy landed at its destination engaged with nobody.
  it "engages an investigator at the destination when a card effect moves it out of a threat area"
    . gameTest
    $ \self -> do
      other <- addInvestigator Investigators.rolandBanks
      location1 <- testLocation
      location2 <- testLocation

      self `moveTo` location1
      other `moveTo` location2

      enemy <- testEnemy
      enemy `spawnAt` location1
      run $ EnemyEngageInvestigator (toId enemy) (toId self)
      self.engagedEnemies `shouldReturn` [toId enemy]

      run $ EnemyMove (toId enemy) (toId location2)

      enemy.location `shouldReturn` Just (toId location2)
      self.engagedEnemies `shouldReturn` []
      other.engagedEnemies `shouldReturn` [toId enemy]

  -- Regression for issue #5313 (Virescent Rot on a Nightriders host). Engaged
  -- swarm cards are reported alongside their host, and their
  -- `EnemyEnteredFollowing` redirects to the host — so a host that had just
  -- been left behind by `DisengageEnemy` got dragged to the destination and
  -- immediately re-engaged.
  it "leaves a swarming host and its swarm behind when the host cannot move"
    . gameTest
    $ \self -> do
      location1 <- testLocation
      location2 <- testLocation
      self `moveTo` location1

      (host, swarm) <- swarmingEnemyEngagedWith self location1
      run =<< gameModifier (TestSource mempty) (toTarget host) CannotMove

      self `moveTo` location2

      host.location `shouldReturn` Just (toId location1)
      swarm.location `shouldReturn` Just (toId location1)
      self.engagedEnemies `shouldReturn` []

  -- Swarming X: "The host enemy and all of its swarm cards move, engage, and
  -- exhaust as a single entity", so a swarm card that cannot move holds the
  -- whole group back.
  it "leaves a swarming host behind when one of its swarm cards cannot move"
    . gameTest
    $ \self -> do
      location1 <- testLocation
      location2 <- testLocation
      self `moveTo` location1

      (host, swarm) <- swarmingEnemyEngagedWith self location1
      run =<< gameModifier (TestSource mempty) (EnemyTarget swarm) CannotMove

      self `moveTo` location2

      host.location `shouldReturn` Just (toId location1)
      swarm.location `shouldReturn` Just (toId location1)
      self.engagedEnemies `shouldReturn` []

  it "moves a swarming host and its swarm along with the investigator"
    . gameTest
    $ \self -> do
      location1 <- testLocation
      location2 <- testLocation
      self `moveTo` location1

      (host, swarm) <- swarmingEnemyEngagedWith self location1

      self `moveTo` location2

      host.location `shouldReturn` Just (toId location2)
      swarm.location `shouldReturn` Just (toId location2)
      engaged <- self.engagedEnemies
      sort engaged `shouldBe` sort [host, swarm]

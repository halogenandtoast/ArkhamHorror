module Arkham.Enemy.EngagementSpec (spec) where

import Arkham.Investigator.Cards qualified as Investigators
import TestImport.New

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

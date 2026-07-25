module Arkham.Enemy.Cards.BatHorrorSpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Event.Cards qualified as Events
import Arkham.Location.Base (
  connectedMatchersL,
  revealedConnectedMatchersL,
  revealedSymbolL,
  symbolL,
 )
import Arkham.Location.Cards qualified as Locations
import Arkham.LocationSymbol
import Arkham.Matcher (LocationMatcher (LocationWithSymbol))
import TestImport.New

spec :: Spec
spec = describe "Bat Horror" do
  context "Elusive" do
    -- Regression for issue #5249. The attack happened inside the *enter window*
    -- of the very move that brought the enemy here, so the outer move's deferred
    -- placement write was still queued when the elusive flee committed and it
    -- dragged the enemy back to where it fled from.
    it "stays where it fled when attacked during the move that brought it here" . gameTest $ \self -> do
      location1 <-
        testLocationWithDef Locations.study
          $ (symbolL .~ Square)
          . (revealedSymbolL .~ Square)
          . (connectedMatchersL .~ [LocationWithSymbol Triangle, LocationWithSymbol Moon])
          . (revealedConnectedMatchersL .~ [LocationWithSymbol Triangle, LocationWithSymbol Moon])
      location2 <-
        testLocationWithDef Locations.rivertown
          $ (symbolL .~ Triangle)
          . (revealedSymbolL .~ Triangle)
          . (connectedMatchersL .~ [LocationWithSymbol Square])
          . (revealedConnectedMatchersL .~ [LocationWithSymbol Square])
      location3 <-
        testLocationWithDef Locations.southsideHistoricalSociety
          $ (symbolL .~ Moon)
          . (revealedSymbolL .~ Moon)
          . (connectedMatchersL .~ [LocationWithSymbol Square])
          . (revealedConnectedMatchersL .~ [LocationWithSymbol Square])

      self `moveTo` location1
      self `playEvent` Events.lieInWait

      batHorror <- testEnemyWithDef Enemies.batHorror id & prop @"fight" 1 & prop @"health" 5
      batHorror `spawnAt` location2

      run $ EnemyMove (toId batHorror) (toId location1)
      useReaction -- Lie in Wait: fight the enemy that just entered
      chooseSkill #combat
      chooseOptionMatching "fight Bat Horror" $ \case
        FightLabel {enemyId} -> enemyId == toId batHorror
        _ -> False
      setChaosTokens [AutoFail] -- elusive fires whether or not the attack lands
      chooseTarget location3 -- elusive: flee to a connecting location
      batHorror.location `shouldReturn` Just (toId location3)
      batHorror.exhausted `shouldReturn` True

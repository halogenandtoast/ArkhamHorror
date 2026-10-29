module Arkham.Location.Cards.TheScarletKeys.CongressOfTheKeys.CongressChamberLairSpec (spec) where

import Arkham.Enemy.CardDefs.TheScarletKeys.CleanupCrew qualified as Enemies
import Arkham.Enemy.Types (EnemyAttrs (..))
import Arkham.Location.CardDefs.TheScarletKeys.CongressOfTheKeys qualified as Locations
import Arkham.Location.Types (revealedL)
import Arkham.Placement
import TestImport.New

{- | "While there is a ready Coterie enemy at Congress Chamber, increase the cost of
each asset played by each investigator at Congress Chamber by 2."
-}
spec :: Spec
spec = describe "Congress Chamber (Lair)" do
  it "increases asset costs when a ready Coterie enemy is at it" . gameTest $ \self -> do
    chamber <- testLocationWithDef Locations.congressChamberLair (revealedL .~ True)
    self `moveTo` chamber
    _ <- testEnemyWithDef Enemies.coterieAssassinA \a -> a {enemyPlacement = AtLocation (toId chamber)}
    tick
    getModifiers self `shouldContainM` [IncreaseCostOf #asset 2]

  it "does not increase asset costs when the only ready Coterie enemy is elsewhere" . gameTest $ \self -> do
    chamber <- testLocationWithDef Locations.congressChamberLair (revealedL .~ True)
    elsewhere <- testLocation
    self `moveTo` chamber
    _ <- testEnemyWithDef Enemies.coterieAssassinA \a -> a {enemyPlacement = AtLocation (toId elsewhere)}
    tick
    getModifiers self `shouldNotContainM` [IncreaseCostOf #asset 2]

  it "does not increase asset costs when the Coterie enemy at it is exhausted" . gameTest $ \self -> do
    chamber <- testLocationWithDef Locations.congressChamberLair (revealedL .~ True)
    self `moveTo` chamber
    _ <-
      testEnemyWithDef Enemies.coterieAssassinA \a ->
        a {enemyPlacement = AtLocation (toId chamber), enemyExhausted = True}
    tick
    getModifiers self `shouldNotContainM` [IncreaseCostOf #asset 2]

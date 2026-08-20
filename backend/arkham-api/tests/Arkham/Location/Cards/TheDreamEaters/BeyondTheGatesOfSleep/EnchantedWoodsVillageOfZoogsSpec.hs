module Arkham.Location.Cards.TheDreamEaters.BeyondTheGatesOfSleep.EnchantedWoodsVillageOfZoogsSpec (spec) where

import Arkham.Enemy.CardDefs.TheDreamEaters.TheSearchForKadath qualified as Enemies
import Arkham.ForMovement
import Arkham.Location.CardDefs.TheDreamEaters.BeyondTheGatesOfSleep qualified as Locations
import Arkham.Location.Types (Field (LocationPrintedSymbol), revealedL)
import Arkham.LocationSymbol
import Arkham.Matcher (pattern ConnectedLocation)
import Arkham.Projection (field)
import TestImport.New

spec :: Spec
spec = describe "Enchanted Woods (Village of Zoogs)" do
  it "uses Diamond as its revealed symbol, while Great Stone Circle remains Triangle"
    . gameTest
    $ \_self -> do
      village <- testLocationWithDef Locations.enchantedWoodsVillageOfZoogs (revealedL .~ True)
      greatStoneCircle <- testLocationWithDef Locations.enchantedWoodsGreatStoneCircle (revealedL .~ True)

      field LocationPrintedSymbol (toId village) `shouldReturn` Diamond
      field LocationPrintedSymbol (toId greatStoneCircle) `shouldReturn` Triangle

  it "is connected to The Moon-Tree and gives Inconspicuous Zoog multiple legal spawn choices"
    . gameTest
    $ \self -> do
      moonTree <- testLocationWithDef Locations.enchantedWoodsTheMoonTree (revealedL .~ True)
      village <- testLocationWithDef Locations.enchantedWoodsVillageOfZoogs (revealedL .~ True)
      enchantedPath <- testLocationWithDef Locations.theEnchantedPath (revealedL .~ True)

      self `moveTo` moonTree

      moonTree.connectedLocations `shouldMatchListM` [toId enchantedPath, toId village]

      zoog <- testEnemyWithDef Enemies.inconspicuousZoog id
      run $ EnemySpawnAtLocationMatching (Just $ toId self) (ConnectedLocation NotForMovement) (toId zoog)

      assertTarget enchantedPath
      assertTarget village
      chooseTarget village

      zoog.location `shouldReturn` Just (toId village)

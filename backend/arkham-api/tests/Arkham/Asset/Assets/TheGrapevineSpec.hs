module Arkham.Asset.Assets.TheGrapevineSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Base (
  connectedMatchersL,
  revealedConnectedMatchersL,
  revealedSymbolL,
  symbolL,
  revealedL,
 )
import Arkham.LocationSymbol
import Arkham.Matcher (LocationMatcher (LocationWithSymbol))
import TestImport.New

{- | location1 <-> location2 <-> location3. All revealed: The Grapevine can only
target an enemy at a revealed location, and test locations default to unrevealed.
-}
threeInARow :: TestAppT (Location, Location, Location)
threeInARow = do
  location1 <-
    testLocationWith
      $ (revealedL .~ True)
      . (symbolL .~ Square)
      . (revealedSymbolL .~ Square)
      . (connectedMatchersL .~ [LocationWithSymbol Triangle])
      . (revealedConnectedMatchersL .~ [LocationWithSymbol Triangle])
  location2 <-
    testLocationWith
      $ (revealedL .~ True)
      . (symbolL .~ Triangle)
      . (revealedSymbolL .~ Triangle)
      . (connectedMatchersL .~ [LocationWithSymbol Square, LocationWithSymbol Moon])
      . (revealedConnectedMatchersL .~ [LocationWithSymbol Square, LocationWithSymbol Moon])
  location3 <-
    testLocationWith
      $ (revealedL .~ True)
      . (symbolL .~ Moon)
      . (revealedSymbolL .~ Moon)
      . (connectedMatchersL .~ [LocationWithSymbol Triangle])
      . (revealedConnectedMatchersL .~ [LocationWithSymbol Triangle])
  pure (location1, location2, location3)

spec :: Spec
spec = describe "The Grapevine" do
  context "action" do
    it "moves you one location at a time to the enemy and engages it" . gameTest $ \self -> do
      grapevine <- self `putAssetIntoPlay` Assets.theGrapevine
      (location1, location2, location3) <- threeInARow
      self `moveTo` location1
      enemy <- testEnemy
      enemy `spawnAt` location3
      -- aloof, so only the card's explicit engage can engage it
      run =<< gameModifier (TestSource mempty) (toTarget enemy) (AddKeyword Keyword.Aloof)

      [action] <- self `getActionsFrom` grapevine
      self `useAbility` action
      chooseTarget enemy
      chooseTarget location2
      chooseTarget location3

      self.location `shouldReturn` Just (toId location3)
      enemy.location `shouldReturn` Just (toId location3)
      self.engagedEnemies `shouldReturn` [toId enemy]

    -- Regression for issue #5353. The engage used to be queued alongside the move, so
    -- when the move was stopped short (there: Whispers in Your Head (Dread)) the engage
    -- still ran and dragged the enemy across the board into the investigator's location.
    it "does not drag the enemy to you when your movement is stopped short" . gameTest $ \self -> do
      grapevine <- self `putAssetIntoPlay` Assets.theGrapevine
      (location1, _location2, location3) <- threeInARow
      self `moveTo` location1
      enemy <- testEnemy
      enemy `spawnAt` location3
      run =<< gameModifier (TestSource mempty) (toTarget self) CannotMove

      [action] <- self `getActionsFrom` grapevine
      self `useAbility` action
      chooseTarget enemy

      self.location `shouldReturn` Just (toId location1)
      enemy.location `shouldReturn` Just (toId location3)
      self.engagedEnemies `shouldReturn` []

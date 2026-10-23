module Arkham.Asset.Assets.TheGrapevineSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Base (
  connectedMatchersL,
  revealedConnectedMatchersL,
  revealedL,
  revealedSymbolL,
  symbolL,
 )
import Arkham.LocationSymbol
import Arkham.Matcher (LocationMatcher (Anywhere, LocationWithSymbol))
import Arkham.Token (Token (Clue))
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

  -- Regression for issue #5894. The path steps were chosen straight off
  -- ClosestPathLocation, so a location the group could not pay to enter
  -- (Underground Cistern's group clue cost) was offered as a legal step -- and
  -- the unpayable cost then resolved to nothing, letting you in for free.
  context "a location you cannot pay to enter" do
    it "is not offered as a step along the path" . gameTest $ \self -> do
      grapevine <- self `putAssetIntoPlay` Assets.theGrapevine
      (location1, location2, location3) <- threeInARow
      self `moveTo` location1
      enemy <- testEnemy
      enemy `spawnAt` location3
      run
        =<< gameModifier
          (TestSource mempty)
          (toTarget location2)
          (AdditionalCostToEnter $ GroupClueCost (Static 3) Anywhere)

      [action] <- self `getActionsFrom` grapevine
      self `useAbility` action
      chooseTarget enemy

      self.location `shouldReturn` Just (toId location1)
      self.clues `shouldReturn` 0
      enemy.location `shouldReturn` Just (toId location3)
      self.engagedEnemies `shouldReturn` []

    it "is a step along the path once the cost can be paid" . gameTest $ \self -> do
      grapevine <- self `putAssetIntoPlay` Assets.theGrapevine
      (location1, location2, location3) <- threeInARow
      self `moveTo` location1
      enemy <- testEnemy
      enemy `spawnAt` location3
      run $ PlaceTokens (TestSource mempty) (toTarget self) Clue 3
      run
        =<< gameModifier
          (TestSource mempty)
          (toTarget location2)
          (AdditionalCostToEnter $ GroupClueCost (Static 3) Anywhere)

      [action] <- self `getActionsFrom` grapevine
      self `useAbility` action
      chooseTarget enemy
      chooseTarget location2
      chooseTarget location3

      self.location `shouldReturn` Just (toId location3)
      self.clues `shouldReturn` 0
      self.engagedEnemies `shouldReturn` [toId enemy]

    it "is not entered by a direct move either" . gameTest $ \self -> do
      (location1, location2, _location3) <- threeInARow
      self `moveTo` location1
      run
        =<< gameModifier
          (TestSource mempty)
          (toTarget location2)
          (AdditionalCostToEnter $ GroupClueCost (Static 3) Anywhere)

      self `moveTo` location2

      self.location `shouldReturn` Just (toId location1)
      self.clues `shouldReturn` 0

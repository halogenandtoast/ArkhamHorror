module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Treacheries.CallOfTheSeaSpec (spec) where

import Arkham.Helpers.Location (getCanForcedStepTo, getCanStepTo)
import Arkham.Helpers.Message qualified as Helpers
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as Treacheries
import Arkham.Location.Base (
  connectedMatchersL,
  revealedConnectedMatchersL,
  revealedL,
  revealedSymbolL,
  symbolL,
 )
import Arkham.Location.FloodLevel
import Arkham.LocationSymbol
import Arkham.Matcher
import Arkham.Placement (Placement (InThreatArea))
import TestImport.New

{- | A dry location (Square) connected to a fully flooded one (Triangle), standing in for
Lonely Isle and Churning Waters on Devil Reef.
-}
twoInARow :: TestAppT (Location, Location)
twoInARow = do
  dry <-
    testLocationWith
      $ (revealedL .~ True)
      . (symbolL .~ Square)
      . (revealedSymbolL .~ Square)
      . (connectedMatchersL .~ [LocationWithSymbol Triangle])
      . (revealedConnectedMatchersL .~ [LocationWithSymbol Triangle])
  flooded <-
    testLocationWith
      $ (revealedL .~ True)
      . (symbolL .~ Triangle)
      . (revealedSymbolL .~ Triangle)
      . (connectedMatchersL .~ [LocationWithSymbol Square])
      . (revealedConnectedMatchersL .~ [LocationWithSymbol Square])
  run $ SetFloodLevel (toId flooded) FullyFlooded
  pure (dry, flooded)

{- | The Devil of the Depths (07166): "As an additional cost for you to move from one Ocean
location to another while not in a vehicle, you must spend 2 actions."
-}
surcharge :: Location -> TestAppT ()
surcharge flooded =
  run =<< gameModifier (TestSource mempty) (toTarget flooded) (AdditionalCostToEnter $ ActionCost 2)

spec :: Spec
spec = describe "Call of the Sea" do
  -- Regression for issue #5968. The Forced move went through MoveToward, whose step walk
  -- and `CanMoveCloserToLocation` criterion both gate on *affording* the additional costs
  -- (#5894). At end of turn there are no actions left, so The Devil of the Depths' 2-action
  -- surcharge on entering an Ocean location silenced the ability entirely. "Additional
  -- costs are not paid when a mandatory instruction requires an investigator to resolve an
  -- effect" (Rules Reference, "Additional Costs").
  context "Forced - after your turn ends" do
    it "moves you to the fully flooded location despite the agenda's additional cost" . gameTest $ \self -> do
      (dry, flooded) <- twoInARow
      self `moveTo` dry
      surcharge flooded
      callOfTheSea <- genCard Treacheries.callOfTheSea
      run =<< Helpers.createTreacheryAt_ callOfTheSea (InThreatArea self.id)
      run $ SetActions self.id (TestSource mempty) 0

      run $ chooseEndTurn self
      useForcedAbility
      chooseTarget flooded

      self.location `shouldReturn` Just (toId flooded)
      -- The surcharge was waived, not paid out of some other pool.
      self.remainingActions `shouldReturn` 0

    it "still does nothing while you cannot move" . gameTest $ \self -> do
      (dry, flooded) <- twoInARow
      self `moveTo` dry
      surcharge flooded
      callOfTheSea <- genCard Treacheries.callOfTheSea
      run =<< Helpers.createTreacheryAt_ callOfTheSea (InThreatArea self.id)
      run $ SetActions self.id (TestSource mempty) 0
      run =<< gameModifier (TestSource mempty) (toTarget self) CannotMove

      run $ chooseEndTurn self

      self.location `shouldReturn` Just (toId dry)

  context "the step query" do
    it "ignores an unaffordable additional cost only on the forced path" . gameTest $ \self -> do
      (dry, flooded) <- twoInARow
      self `moveTo` dry
      surcharge flooded
      run $ SetActions self.id (TestSource mempty) 0

      getCanStepTo self.id (TestSource mempty) (toId flooded) `shouldReturn` False
      getCanForcedStepTo self.id (TestSource mempty) (toId flooded) `shouldReturn` True

    it "still refuses a step the forced move is not allowed to take" . gameTest $ \self -> do
      (dry, flooded) <- twoInARow
      self `moveTo` dry
      -- Barricades are read off the location being left, naming what cannot be entered.
      run
        =<< gameModifier (TestSource mempty) (toTarget dry) (Barricades [toId flooded])

      getCanForcedStepTo self.id (TestSource mempty) (toId flooded) `shouldReturn` False

  context "the ability criterion" do
    it "sees the flooded location on the forced path but not the voluntary one" . gameTest $ \self -> do
      (dry, flooded) <- twoInARow
      self `moveTo` dry
      surcharge flooded
      run $ SetActions self.id (TestSource mempty) 0

      selectAny (CanMoveCloserToLocation (TestSource mempty) You FullyFloodedLocation)
        `shouldReturn` False
      selectAny (CanBeMovedCloserToLocation (TestSource mempty) You FullyFloodedLocation)
        `shouldReturn` True

module Arkham.Helpers.CostSpec (spec) where

import Arkham.Helpers.Cost (getCanAffordCost)
import Arkham.Helpers.Location (getCanStepTo)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Base (costToEnterUnrevealedL)
import Arkham.Matcher (LocationMatcher (..))
import TestImport.New

spec :: Spec
spec = describe "combined clue cost affordability" do
  -- Rear Corridors costs 3/player from Side Building, plus The Boundary's
  -- 1/player from anywhere. The printed cost is wrapped in the move ability.
  for_ [(3, False), (4, True)] \(clues, affordable) -> do
    it ("requires the full four clues with " <> show clues <> " available") . gameTest $ \self -> do
      sideBuilding <- testLocation
      self `moveTo` sideBuilding
      withProp @"clues" clues self
      let cost =
            Costs
              [ NonBlankedCost
                  $ CostToEnterUnrevealed
                  $ GroupClueCost (PerPlayer 3) (LocationWithId sideBuilding.id)
              , GroupClueCost (PerPlayer 1) Anywhere
              ]
      getCanAffordCost self.id (TestSource mempty) [] [] cost `shouldReturn` affordable

    it ("checks the combined entry cost before offering a step with " <> show clues <> " clues") . gameTest $ \self -> do
      sideBuilding <- testLocation
      rearCorridors <-
        testLocationWith
          (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 3) (LocationWithId sideBuilding.id))
      self `moveTo` sideBuilding
      withProp @"clues" clues self
      run
        =<< gameModifier
          (TestSource mempty)
          (toTarget self)
          ( AdditionalCostToEnterMatching (LocationWithId rearCorridors.id)
              $ GroupClueCost (PerPlayer 1) Anywhere
          )
      getCanStepTo self.id (TestSource mempty) rearCorridors.id `shouldReturn` affordable

  it "allows the unrestricted surcharge to be paid by another investigator" . gameTest $ \self -> do
    other <- addInvestigator Investigators.rolandBanks
    sideBuilding <- testLocation
    elsewhere <- testLocation
    self `moveTo` sideBuilding
    other `moveTo` elsewhere
    withProp @"clues" 6 self
    withProp @"clues" 2 other
    let cost =
          Costs
            [ GroupClueCost (PerPlayer 3) (LocationWithId sideBuilding.id)
            , GroupClueCost (PerPlayer 1) Anywhere
            ]
    getCanAffordCost self.id (TestSource mempty) [] [] cost `shouldReturn` True

  it "does not use clues elsewhere to cover a shortage in the restricted pool" . gameTest $ \self -> do
    other <- addInvestigator Investigators.rolandBanks
    sideBuilding <- testLocation
    elsewhere <- testLocation
    self `moveTo` sideBuilding
    other `moveTo` elsewhere
    withProp @"clues" 5 self
    withProp @"clues" 3 other
    let cost =
          Costs
            [ GroupClueCost (PerPlayer 3) (LocationWithId sideBuilding.id)
            , GroupClueCost (PerPlayer 1) Anywhere
            ]
    getCanAffordCost self.id (TestSource mempty) [] [] cost `shouldReturn` False

  it "cannot reuse an investigator's clues for a personal and a group cost" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    withProp @"clues" 3 self
    let cost = Costs [ClueCost (Static 3), GroupClueCost (Static 1) Anywhere]
    getCanAffordCost self.id (TestSource mempty) [] [] cost `shouldReturn` False

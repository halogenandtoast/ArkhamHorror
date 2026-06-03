module Arkham.Matcher.NearestLocationToMostSpec (spec) where

import Arkham.Entities qualified as Entities
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Types (connectedMatchersL, revealedConnectedMatchersL)
import Arkham.Matcher.Location (LocationMatcher (..))
import TestImport.New

-- Re-read each location from game state before patching it; `updateThis` writes
-- the locally-modified value, so chaining multiple `connect` calls on the same
-- location with stale references would silently drop earlier edits.
addConnection :: LocationId -> LocationId -> TestAppT ()
addConnection from to' =
  overTest
    $ entitiesL
    . Entities.locationsL
    . ix from
    %~ overAttrs
      ( (connectedMatchersL <>~ [LocationWithId to'])
          . (revealedConnectedMatchersL <>~ [LocationWithId to'])
      )

connect :: Location -> Location -> TestAppT ()
connect a b = do
  addConnection (toId a) (toId b)
  addConnection (toId b) (toId a)

anyOf :: [Location] -> LocationMatcher
anyOf = LocationMatchAny . map (LocationWithId . toId)

-- `addInvestigator` only inserts into entities; `getInvestigators` filters by
-- `gamePlayerOrder` via `inTurnOrder`, so additions must be appended to the
-- player order for them to show up in queries.
addInvestigatorInTurnOrder :: CardDef -> TestAppT Investigator
addInvestigatorInTurnOrder def = do
  i <- addInvestigator def
  overTest $ \g -> g {gamePlayerOrder = gamePlayerOrder g <> [toId i]}
  pure i

spec :: Spec
spec = describe "NearestLocationToMost" do
  it "picks the candidate whose worst-case distance to any investigator is smallest" . gameTest $ \self -> do
    -- Chain: l1 - l2 - l3 - l4 - l5
    -- self + i2 at l1; i3 at l5.
    -- Candidates: l1, l3, l5.
    --   l1: max distance to investigators = max(0,0,4) = 4
    --   l3: max distance = max(2,2,2)               = 2
    --   l5: max distance = max(4,4,0)               = 4
    -- Min of those maxes is 2 -> l3 wins (the "median" location).
    i2 <- addInvestigatorInTurnOrder Investigators.rolandBanks
    i3 <- addInvestigatorInTurnOrder Investigators.daisyWalker
    l1 <- testLocation
    l2 <- testLocation
    l3 <- testLocation
    l4 <- testLocation
    l5 <- testLocation
    connect l1 l2
    connect l2 l3
    connect l3 l4
    connect l4 l5
    self `moveTo` l1
    i2 `moveTo` l1
    i3 `moveTo` l5
    select (NearestLocationToMost (anyOf [l1, l3, l5])) `shouldMatchListM` [toId l3]

  it "ties across disconnected components when their worst-case distances are equal" . gameTest $ \self -> do
    -- Component A: l1 - l2 (self at l1, candidate l2 at distance 1)
    -- Component B: l3 - l4 (i2  at l3, candidate l4 at distance 1)
    -- Each candidate is reachable only by the investigators in its own component
    -- (max = 1 each). With equal maxes both components tie and both are returned.
    i2 <- addInvestigatorInTurnOrder Investigators.rolandBanks
    l1 <- testLocation
    l2 <- testLocation
    l3 <- testLocation
    l4 <- testLocation
    connect l1 l2
    connect l3 l4
    self `moveTo` l1
    i2 `moveTo` l3
    select (NearestLocationToMost (anyOf [l2, l4])) `shouldMatchListM` [toId l2, toId l4]

  it "an unreachable candidate is preferred when its reachable-side max is smaller" . gameTest $ \self -> do
    -- Component A: l1 - l2          (self at l1, candidate l2 distance 1, max = 1)
    -- Component B: l3 - l4 - l5     (i2   at l3, candidate l5 distance 2, max = 2)
    -- l2 has the smaller max even though i2 cannot reach it -> [l2].
    i2 <- addInvestigatorInTurnOrder Investigators.rolandBanks
    l1 <- testLocation
    l2 <- testLocation
    l3 <- testLocation
    l4 <- testLocation
    l5 <- testLocation
    connect l1 l2
    connect l3 l4
    connect l4 l5
    self `moveTo` l1
    i2 `moveTo` l3
    select (NearestLocationToMost (anyOf [l2, l5])) `shouldMatchListM` [toId l2]

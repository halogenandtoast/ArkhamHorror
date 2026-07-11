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

spec :: Spec
spec = describe "NearestLocationToMost" do
  it "counts each investigator's nearest candidate and picks the most-voted" . gameTest $ \self -> do
    -- Chain: l1 - l2 - l3 - l4 - l5
    -- self + i2 at l1; i3 at l5. Candidates: l1, l3, l5.
    --   self -> nearest candidate l1 (0)   | i2 -> l1 (0)  | i3 -> l5 (0)
    -- Votes: l1 = 2, l5 = 1, l3 = 0 -> l1 wins.
    i2 <- addInvestigator Investigators.rolandBanks
    i3 <- addInvestigator Investigators.daisyWalker
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
    select (NearestLocationToMost (anyOf [l1, l3, l5])) `shouldMatchListM` [toId l1]

  it "ties across disconnected components when each gets one vote" . gameTest $ \self -> do
    -- Component A: l1 - l2 (self at l1, votes l2)
    -- Component B: l3 - l4 (i2  at l3, votes l4)
    -- Each candidate collects one vote -> both tie.
    i2 <- addInvestigator Investigators.rolandBanks
    l1 <- testLocation
    l2 <- testLocation
    l3 <- testLocation
    l4 <- testLocation
    connect l1 l2
    connect l3 l4
    self `moveTo` l1
    i2 `moveTo` l3
    select (NearestLocationToMost (anyOf [l2, l4])) `shouldMatchListM` [toId l2, toId l4]

  it "ties on votes even when one candidate is farther for its voter" . gameTest $ \self -> do
    -- Component A: l1 - l2          (self at l1, votes l2 at distance 1)
    -- Component B: l3 - l4 - l5     (i2   at l3, votes l5 at distance 2)
    -- Distance is irrelevant across investigators: each candidate has one vote.
    i2 <- addInvestigator Investigators.rolandBanks
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
    select (NearestLocationToMost (anyOf [l2, l5])) `shouldMatchListM` [toId l2, toId l5]

  it "ties all candidates that are some investigator's nearest (issue #5062)" . gameTest $ \self -> do
    -- Layout:  a - d - x, with x - b and x - c.
    -- self on candidate a; i2 on x, equidistant (1) from candidates b and c.
    --   self -> a (0)                | i2 -> b (1) and c (1), a is 2 away
    -- Votes: a = 1, b = 1, c = 1 -> all three tie (lead investigator decides).
    -- Mirrors the reported game: an investigator standing on a Coastal location
    -- plus another equidistant from two others must offer all three as choices.
    i2 <- addInvestigator Investigators.rolandBanks
    a <- testLocation
    d <- testLocation
    x <- testLocation
    b <- testLocation
    c <- testLocation
    connect a d
    connect d x
    connect x b
    connect x c
    self `moveTo` a
    i2 `moveTo` x
    select (NearestLocationToMost (anyOf [a, b, c])) `shouldMatchListM` [toId a, toId b, toId c]

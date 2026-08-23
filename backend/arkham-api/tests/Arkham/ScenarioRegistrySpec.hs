module Arkham.ScenarioRegistrySpec (spec) where

import Arkham.Card.CardCode (unCardCode)
import Arkham.Prelude
import Arkham.Scenario.Scenarios (allScenarioBuilders)
import Arkham.Scenario.Types (someScenarioCardCode)
import Test.Hspec

-- allScenarios is derived from the card code each builder carries in its own
-- attrs, so two builders sharing a code silently drop one of them from the
-- registry and any campaign step pointing at the lost code dies with "Unknown
-- scenario" (#5491: Heart of the Elders, Part 2 still declared "04205a").
--
-- unCardCode because Eq CardCode deliberately matches the two sides of a card,
-- so "04205a" and "04205b" compare equal; the registry is a Map keyed by the
-- exact Ord, and that is what must be unique.
spec :: Spec
spec = describe "scenario registry" do
  it "gives every scenario builder its own card code" do
    let codes = map (unCardCode . someScenarioCardCode) allScenarioBuilders
    (codes \\ nub codes) `shouldBe` []

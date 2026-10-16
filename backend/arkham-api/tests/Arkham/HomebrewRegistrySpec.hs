module Arkham.HomebrewRegistrySpec (spec) where

import Arkham.Card.CardCode (toCardCode)
import Arkham.Homebrew.Defs (allHomebrewDefs)
import Arkham.Homebrew.DefsBase (hdInvestigators)
import Arkham.Homebrew.Registry qualified as Registry
import Arkham.Investigator (allInvestigators)
import Data.Map.Strict qualified as Map
import Test.Hspec
import Prelude

-- Instance discovery is silent-by-construction: if the aggregators or TH fold
-- break, everything compiles and homebrew content just vanishes. These
-- assertions are deliberately content-independent — adding or removing a
-- campaign must not touch this file.
spec :: Spec
spec = describe "Homebrew instance discovery" do
  it "discovers campaigns, scenarios, and cards at all" do
    -- length, not the lists themselves: these registry types have no Show
    length Registry.campaigns `shouldSatisfy` (> 0)
    length Registry.scenarios `shouldSatisfy` (> 0)
    length Registry.locations `shouldSatisfy` (> 0)
    length Registry.enemies `shouldSatisfy` (> 0)

  it "pairs every discovered scenario with its encounter set" do
    map fst Registry.scenarios `shouldMatchList` map fst Registry.scenarioSets

  -- Investigators are the one slot a campaign is unlikely to fill, so there is
  -- no "> 0" to assert. What can be asserted is that the two halves of the door
  -- agree: a discovered definition has to reach an implementation, or
  -- lookupInvestigator errors the first time the card is built. Vacuous while
  -- no campaign ships one.
  it "backs every discovered homebrew investigator definition with an implementation" do
    let unimplemented =
          [ code
          | def <- hdInvestigators allHomebrewDefs
          , let code = toCardCode def
          , not (Map.member code allInvestigators)
          ]
    unimplemented `shouldBe` []

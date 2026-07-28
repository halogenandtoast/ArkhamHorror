module Arkham.Matcher.LegacyJSONSpec (spec) where

import Arkham.Matcher
import Data.Aeson (decode, encode)
import Test.Hspec
import Prelude

spec :: Spec
spec = describe "matcher legacy JSON" do
  -- Issue #5262: InvestigatorCanDiscoverCluesAtOneOf was renamed to
  -- InvestigatorWithDiscoverableCluesAt (and corrected to require that there is
  -- actually something to discover). Games saved before the rename still carry
  -- the old tag inside parked ability criteria, so it has to keep parsing.
  -- payload copied verbatim out of the export attached to #5262
  it "remaps the legacy InvestigatorCanDiscoverCluesAtOneOf constructor name" do
    decode
      "{\"tag\":\"InvestigatorCanDiscoverCluesAtOneOf\",\"contents\":{\"tag\":\"LocationWithInvestigator\",\"contents\":{\"tag\":\"You\"}}}"
      `shouldBe` Just (InvestigatorWithDiscoverableCluesAt YourLocation)

  it "round-trips the current constructor" do
    decode (encode $ InvestigatorWithDiscoverableCluesAt YourLocation)
      `shouldBe` Just (InvestigatorWithDiscoverableCluesAt YourLocation)

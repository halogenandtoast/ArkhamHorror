module Arkham.Campaign.NightOfTheZealotSpec ( spec) where

import Arkham.Scenario.SpecHelpers
import TestImport

spec :: Spec
spec = describe "Night of the Zealot" do
  context "The Gathering" $ encounterDeckCountIs "01104" 26
  context "The Midnight Masks" $ encounterDeckCountIs "01120" 21
  context "The Devouer Below" $ encounterDeckCountIs "01142" 29

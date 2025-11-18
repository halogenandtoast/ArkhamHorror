module Arkham.Campaign.TheDunwichLegacySpec (spec) where

import Arkham.Scenario.SpecHelpers
import TestImport

spec :: Spec
spec = describe "The Dunwich Legacy" do
  context "Extracurricular Activity" $ encounterDeckCountIs "02041" 32
  context "The House Always Wins" $ encounterDeckCountIs "02062" 19
  context "The Miskatonic Museum" $ encounterDeckCountIs "02118" 34
  context "The Essex Country Express" $ encounterDeckCountIs "02159" 35
  context "Blood on the Altar" $ encounterDeckCountIs "02195" 26 -- 3 are removed during setup
  context "Undimensioned and Unseen" $ encounterDeckCountIs "02236" 33
  context "Where Doom Awaits" $ encounterDeckCountIs "02274" 44
  context "Lost in Time and Space" $ encounterDeckCountIs "02311" 42

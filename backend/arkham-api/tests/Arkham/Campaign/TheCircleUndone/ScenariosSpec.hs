module Arkham.Campaign.TheCircleUndone.ScenariosSpec (spec) where

import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.Scenario.SpecHelpers
import TestImport

spec :: Spec
spec = describe "The Circle Undone" do
  context "The Witching Hour" $ encounterDeckCountIs "05050" 26
  context "At Death's Doorstep" $ encounterDeckCountIs "05065" 25
  context "The Secret Name" $ encounterDeckCountIs "05120" 35
  context "The Wages of Sin" $ encounterDeckCountIs "05161" 24
  context "For the Greater Good" do
    encounterDeckCountIs "05197" 29
    context "Member of the Lodge" do
      encounterDeckCountIsWith "05197" 29 (record TheInvestigatorsAreMembersOfTheLodge)
  context "Union and Disillusion" $ encounterDeckCountIs "05238" 35
  context "In the Clutches of Chaos" do
    context "Anette Mason is Possessed by Evil" do
      encounterDeckCountIsWith "05284" 35 (record AnetteMasonIsPossessedByEvil)
    context "Carl Sanford Possesses the Secrets of the Universe" do
      encounterDeckCountIsWith "05284" 37 (record CarlSanfordPossessesTheSecretsOfTheUniverse)
  context "Before the Black Throne" $ encounterDeckCountIs "05325" 30

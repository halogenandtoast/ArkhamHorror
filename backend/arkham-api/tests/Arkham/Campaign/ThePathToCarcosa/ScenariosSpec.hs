module Arkham.Campaign.ThePathToCarcosa.ScenariosSpec (spec) where

import Arkham.Campaigns.ThePathToCarcosa.Key
import Arkham.Scenario.SpecHelpers
import Arkham.Enemy.Cards qualified as Enemies
import TestImport

spec :: Spec
spec = fdescribe "The Path to Carcosa" do
  context "Curtain Call" $ encounterDeckCountIs "03043" 32
  context "The Last King" $ encounterDeckCountIs "03061" 22
  context "Echoes of the Past" $ encounterDeckCountIs "03120" 33
  context "The Unspeakable Oath" $ encounterDeckCountIs "03159" 23
  context "A Phantom of Truth" do
    context "Equal conviction or more doubt" do
      encounterDeckCountIsWith "03200" 29 (recordCount Doubt 1)
    context "More conviction" do
      encounterDeckCountIsWith "03200" 30 (recordCount Conviction 1)
  context "The Pallid Mask" do
    encounterDeckCountIsWith "03240" 30 (addCampaignCardToDeck Enemies.theManInThePallidMask)
  context "Black Stars Rise" $ encounterDeckCountIs "03274" 30
  context "Dim Carcoa" do
    encounterDeckCountIsWith "03316" 36 (addCampaignCardToDeck Enemies.theManInThePallidMask)

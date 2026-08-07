module Arkham.Investigator.Cards.MarionTavaresSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards qualified as Investigators
import TestImport.New

spec :: Spec
spec = describe "Marion Tavares" do
  context "reaction (after you play an event during your turn)" do
    it "triggers after a normal event play" . gameTestWith Investigators.marionTavares $ \self -> do
      self `loadDeck` [Assets.flashlight]
      emergencyCache <- genMyCard self Events.emergencyCache
      self `addToHand` emergencyCache
      playCard self emergencyCache
      useReaction
      self.resources `shouldReturn` 3
      asDefs self.hand `shouldReturn` [Assets.flashlight]

    -- The Painted World sets cdSkipPlayWindows so it can emit its own #when window for
    -- the event it replaces itself with. That used to swallow the #after window too, so
    -- nothing ever opened "after you play an event". Regression coverage for #5355.
    it "triggers after playing The Painted World" . gameTestWith Investigators.marionTavares $ \self -> do
      self `loadDeck` [Assets.flashlight]
      emergencyCache <- genMyCard self Events.emergencyCache
      run $ PlaceUnderneath (toTarget self) [emergencyCache]
      paintedWorld <- genMyCard self Events.thePaintedWorld
      self `addToHand` paintedWorld
      playCard self paintedWorld
      chooseTarget (toCardId emergencyCache)
      useReaction
      self.resources `shouldReturn` 3
      asDefs self.hand `shouldReturn` [Assets.flashlight]

module Arkham.Asset.Assets.TheBookOfWarSunTzusLegacySpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers.Game (getRemovedFromPlayCards)
import Arkham.Location.Types (revealedL)
import Arkham.Matcher (cardIs)
import TestImport.New

spec :: Spec
spec = describe "The Book of War" do
  it "returns the tactic event to your hand when your turn ends" . gameTest $ \self -> do
    self `putCardIntoPlay` Assets.theBookOfWarSunTzusLegacy
    (location1, _) <- testConnectedLocations (revealedL .~ True) (revealedL .~ True)
    location3 <- testLocation & prop @"revealed" True
    self `moveTo` location1

    duringTurn self do
      self `playEvent` Events.elusive
      chooseTarget location3
      useReaction

    assert $ any (`cardMatch` cardIs Events.elusive) <$> self.hand
    -- the event is *returned*, not removed, so it has to leave the discard and
    -- must never enter the removed-from-game zone, #5697
    assert $ not <$> Events.elusive `isInDiscardOf` self
    assert $ not . any (`cardMatch` cardIs Events.elusive) <$> getRemovedFromPlayCards

module Arkham.Helpers.InvestigatorSpec (spec) where

import Arkham.Helpers.Investigator (getStartingHandSize)
import TestImport.New

spec :: Spec
spec = describe "getStartingHandSize" do
  -- Opening hand size (the number of cards drawn at game start) is base 5,
  -- distinct from the maximum hand size (base 8) returned by getHandSize. The
  -- Feast of Hemlock Vale preludes discard down to this value (issue #4842).
  it "defaults to 5" . gameTest $ \self -> do
    getStartingHandSize self `shouldReturn` 5

  it "respects StartingHand modifiers" . gameTest $ \self -> do
    run =<< gameModifier (TestSource mempty) (toTarget self) (StartingHand 2)
    getStartingHandSize self `shouldReturn` 7

  it "is floored at 0" . gameTest $ \self -> do
    run =<< gameModifier (TestSource mempty) (toTarget self) (StartingHand (-10))
    getStartingHandSize self `shouldReturn` 0

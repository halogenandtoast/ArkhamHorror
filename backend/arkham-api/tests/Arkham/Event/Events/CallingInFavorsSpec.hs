module Arkham.Event.Events.CallingInFavorsSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import Arkham.Matcher
import TestImport.New

spec :: Spec
spec = describe "Calling in Favors" $ do
  -- the deck holds no ally, so the search half finds nothing and the assertions
  -- below only see the return-to-hand half
  it "returns an ally to hand" . gameTest $ \self -> do
    self `loadDeck` [Assets.flashlight, Assets.flashlight]
    beatCop <- self `putAssetIntoPlay` Assets.beatCop
    self `playEvent` Events.callingInFavors
    chooseTarget beatCop
    assert $ selectNone $ assetIs Assets.beatCop
    asDefs self.hand `shouldReturn` [Assets.beatCop]

  it "returns a story ally to hand instead of leaving it in play (#5838)" . gameTest $ \self -> do
    self `loadDeck` [Assets.flashlight, Assets.flashlight]
    elina <- self `putAssetIntoPlay` Assets.elinaHarperKnowsTooMuch
    self `playEvent` Events.callingInFavors
    chooseTarget elina
    assert $ selectNone $ assetIs Assets.elinaHarperKnowsTooMuch
    asDefs self.hand `shouldReturn` [Assets.elinaHarperKnowsTooMuch]

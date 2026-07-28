module Arkham.Asset.Assets.ValeLanternExtinguishedLightSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetLocation))
import Arkham.Deck qualified as Deck
import Arkham.Placement
import Arkham.Projection
import TestImport.New

spec :: Spec
spec = describe "Vale Lantern (Extinguished Light)" do
  context "Forced - When Vale Lantern would leave play: Place it at the nearest location, instead." do
    it "moves to a surviving location when its own location leaves play" . gameTest $ \self -> do
      (doomed, safe) <- testConnectedLocations id id
      self `moveTo` safe
      lantern <- self `putAssetIntoPlay` Assets.valeLanternExtinguishedLight
      run $ PlaceAsset lantern (AtLocation $ toId doomed)
      -- Deepening Dark shuffles unrevealed woods back into the woods deck, which is
      -- what removed the lantern's location out from under it in #5267
      run $ ShuffleCardsIntoTopOfDeck Deck.EncounterDeck 0 [toCard doomed]
      useForcedAbility
      field AssetLocation lantern `shouldReturn` Just (toId safe)

    it "stays put when its location is not going anywhere" . gameTest $ \self -> do
      (here, there) <- testConnectedLocations id id
      self `moveTo` there
      lantern <- self `putAssetIntoPlay` Assets.valeLanternExtinguishedLight
      run $ PlaceAsset lantern (AtLocation $ toId here)
      run $ Discard Nothing GameSource (AssetTarget lantern)
      useForcedAbility
      field AssetLocation lantern `shouldReturn` Just (toId here)

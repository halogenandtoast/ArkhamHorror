module Arkham.Asset.Cards.PatricesViolinSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards (patriceHathaway)
import TestImport.New

spec :: Spec
spec = describe "Patrice's Violin" do
  it
    "Choose and discard 1 card from your hand and exhaust Patrice's Violin: Choose an investigator at your location to either gain 1 resource or draw 1 card."
    . gameTestWith patriceHathaway
    $ \self -> do
      flashlight <- genCard Assets.flashlight
      knife <- genPlayerCard Assets.knife
      withProp @"hand" [flashlight] self
      withProp @"resources" 0 self
      withProp @"deck" (Deck [knife]) self
      patricesViolin <- self `putAssetIntoPlay` Assets.patricesViolin
      location <- testLocation
      self `moveTo` location
      [playViolin] <- self `getActionsFrom` patricesViolin
      self `useAbility` playViolin
      chooseTarget flashlight
      assert patricesViolin.exhausted
      self.discard `shouldReturn` onlyPlayerCards [flashlight]
      withRewind $ do
        chooseOptionMatching "gain 1 resource" \case
          Label "Draw card" _ -> True
          _ -> False
        self.hand `shouldReturn` [toCard knife]

      chooseOptionMatching "draw 1 card" \case
        Label "Gain resource" _ -> True
        _ -> False
      self.resources `shouldReturn` 1

module Arkham.Asset.Assets.FavorOfTheMoon1Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.ChaosToken
import Arkham.Matcher (assetIs)
import Arkham.Projection
import TestImport.New

spec :: Spec
spec = describe "Favor of the Moon" do
  it "stays in play with the curse it seals as it enters play" . gameTest $ \self -> do
    withProp @"resources" 1 self
    setChaosTokens [CurseToken]
    favorOfTheMoon <- genCard Assets.favorOfTheMoon1
    withProp @"hand" [favorOfTheMoon] self
    duringTurn self do
      self `playCard` favorOfTheMoon
      payUpTo 1 1
      chooseOnlyOption "seal curse token"

    favorOfTheMoonId <- selectJust $ assetIs Assets.favorOfTheMoon1
    fieldP AssetSealedChaosTokens ((== 1) . length) favorOfTheMoonId `shouldReturn` True
    asDefs self.discard `shouldReturn` []

  it "discards itself when it seals no tokens" . gameTest $ \self -> do
    withProp @"resources" 1 self
    setChaosTokens [Zero]
    favorOfTheMoon <- genCard Assets.favorOfTheMoon1
    withProp @"hand" [favorOfTheMoon] self
    duringTurn self do
      self `playCard` favorOfTheMoon

    assert $ selectNone $ assetIs Assets.favorOfTheMoon1

module Arkham.Treachery.Cards.RookieMistakeSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards (tommyMuldoon)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Rookie Mistake" $ do
  it "discards each asset with damage on it" . gameTestWith tommyMuldoon $ \self -> do
    beatCop2 <- self `putAssetIntoPlay` Assets.beatCop2
    elderSignAmulet3 <- self `putAssetIntoPlay` Assets.elderSignAmulet3
    self `putCardIntoPlay` Assets.guardDog
    run $ DealAssetDamage beatCop2 (TestSource mempty) 1 0
    run $ DealAssetDamage elderSignAmulet3 (TestSource mempty) 0 1
    self `drawsCard` Treacheries.rookieMistake
    asDefs self.discard
      `shouldMatchListM` [Treacheries.rookieMistake, Assets.beatCop2, Assets.elderSignAmulet3]
    assertAny $ assetIs Assets.guardDog

  it "is shuffled back into your deck if no assets were discarded" . gameTestWith tommyMuldoon $ \self -> do
    self `putCardIntoPlay` Assets.guardDog
    self `drawsCard` Treacheries.rookieMistake
    self.discard `shouldReturn` []
    asDefs self.deck `shouldReturn` [Treacheries.rookieMistake]
    assertAny $ assetIs Assets.guardDog

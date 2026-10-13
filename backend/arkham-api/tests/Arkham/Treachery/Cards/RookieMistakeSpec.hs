module Arkham.Treachery.Cards.RookieMistakeSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetDamage))
import Arkham.Investigator.Cards (tommyMuldoon)
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.CardDefs.TheDreamEaters qualified as Treacheries
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
    loadDeck self [Assets.machete]
    self `putCardIntoPlay` Assets.guardDog
    self `drawsCard` Treacheries.rookieMistake
    self.discard `shouldReturn` []
    asDefs self.deck `shouldContainM` [Treacheries.rookieMistake]
    assertAny $ assetIs Assets.guardDog

  -- A permanent card cannot leave play, so it is neither discarded nor counted
  -- toward "if no assets are discarded by this effect" (#5841).
  it "cannot discard a damaged permanent asset" . gameTestWith tommyMuldoon $ \self -> do
    loadDeck self [Assets.machete]
    toeTheLine <- self `putAssetIntoPlay` Assets.toeTheLine
    run $ PlaceTokens GameSource (toTarget toeTheLine) #damage 4
    self `drawsCard` Treacheries.rookieMistake
    self.discard `shouldReturn` []
    asDefs self.deck `shouldContainM` [Treacheries.rookieMistake]
    assertAny $ assetIs Assets.toeTheLine
    field AssetDamage toeTheLine `shouldReturn` 4

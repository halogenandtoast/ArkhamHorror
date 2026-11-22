module Arkham.Campaign.TheScarletKeys.HollowSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetCard))
import Arkham.Campaigns.TheScarletKeys.Helpers (hollow, pattern HollowedCard)
import Arkham.Helpers.Scenario
import Arkham.Matcher hiding (AssetCard)
import Arkham.Projection (field)
import Arkham.Scenario.Types (Field (..))
import TestImport.New

{- | "Set it aside in the hollow" has to MOVE the card. 'setCardAside' only pulls an
enemy out of play, so a hollowed in-play asset stayed in its owner's play area
while its card also landed in the hollow pile -- Sword Cane and Bandages were in
two places at once after Lost and Forgotten flipped (issue #6041).
-}
spec :: Spec
spec = describe "the hollow" do
  it "takes a hollowed in-play asset out of play" . scenarioTest "09609" $ \self -> do
    swordCane <- self `putAssetIntoPlay` Assets.swordCane
    card <- field AssetCard swordCane
    runQueueT $ hollow (toId self) card
    runMessages

    selectAny (assetIs Assets.swordCane) `shouldReturn` False
    scenarioFieldMap ScenarioSetAsideCards (map toCardId) `shouldReturn` [toCardId card]
    selectMap toCardId HollowedCard `shouldReturn` [toCardId card]

  it "still moves a hollowed card out of hand" . scenarioTest "09609" $ \self -> do
    card <- genCard Assets.swordCane
    self `addToHand` card
    runQueueT $ hollow (toId self) card
    runMessages

    self.hand `shouldReturn` []
    scenarioFieldMap ScenarioSetAsideCards (map toCardId) `shouldReturn` [toCardId card]

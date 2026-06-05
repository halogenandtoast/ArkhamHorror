module Arkham.Asset.Assets.EmbezzledTreasureSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards (rolandBanks)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Name (toTitle)
import Arkham.Projection
import TestImport.New

spec :: Spec
spec = describe "Embezzled Treasure" do
  it "lets you divvy the banked resources among investigators of your choice" . gameTest $ \self -> do
    roland <- addInvestigator rolandBanks
    self `putCardIntoPlay` Assets.embezzledTreasure
    embezzled <- selectJust $ assetIs Assets.embezzledTreasure
    -- 6 resources banked -> 3 bonus resources to distribute (1 per 2 resources)
    run $ PlaceTokens GameSource (toTarget embezzled) #resource 6
    jennyName <- toTitle <$> field InvestigatorName (toId self)
    rolandName <- toTitle <$> field InvestigatorName (toId roland)
    endGame
    useForcedAbility
    resolveAmounts self [(jennyName, 2), (rolandName, 1)]
    getModifiers self `shouldContainM` [StartingResources 2]
    getModifiers roland `shouldContainM` [StartingResources 1]

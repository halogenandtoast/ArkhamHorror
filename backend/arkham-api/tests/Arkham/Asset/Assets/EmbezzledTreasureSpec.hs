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

  -- An investigator who is eliminated still plays the next scenario, so they are
  -- a legal target. 'affectsOthersKnown' used to hide them, leaving an
  -- unanswerable prompt with nothing to distribute to (#5650).
  it "can distribute to an investigator who has been eliminated" . gameTest $ \self -> do
    roland <- addInvestigator rolandBanks
    self `putCardIntoPlay` Assets.embezzledTreasure
    embezzled <- selectJust $ assetIs Assets.embezzledTreasure
    run $ PlaceTokens GameSource (toTarget embezzled) #resource 4
    rolandName <- toTitle <$> field InvestigatorName (toId roland)
    run $ Resign (toId roland)
    endGame
    useForcedAbility
    resolveAmounts self [(rolandName, 2)]
    getModifiers roland `shouldContainM` [StartingResources 2]

  -- Resigning and then ending the scenario opens both of the ability's windows.
  it "distributes only once when you resign and then the game ends" . gameTest $ \self -> do
    self `putCardIntoPlay` Assets.embezzledTreasure
    embezzled <- selectJust $ assetIs Assets.embezzledTreasure
    run $ PlaceTokens GameSource (toTarget embezzled) #resource 4
    jennyName <- toTitle <$> field InvestigatorName (toId self)
    run $ Resign (toId self)
    useForcedAbility
    resolveAmounts self [(jennyName, 2)]
    endGame
    assertHasNoReaction
    getModifiers self `shouldContainM` [StartingResources 2]

module Arkham.Event.Events.DirtyDeeds3Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards (rolandBanks)
import Arkham.Matcher
import TestImport.New

spec :: Spec
spec = describe "Dirty Deeds (3)" do
  -- #5998: the asset was played with a bare PayCardCost, which opens no
  -- Window.PlayCard, so Shrewd Dealings' "when you play an [[Item]] asset" never saw
  -- it and the asset could only ever land under the searcher's control.
  it "lets Shrewd Dealings hand the asset it plays to another investigator (#5998)" . gameTest $ \self -> do
    roland <- addInvestigator rolandBanks
    location <- testLocation
    self `moveTo` location
    roland `moveTo` location
    shrewdDealings <- self `putAssetIntoPlay` Assets.shrewdDealings
    liquidCourageCard <- genPlayerCardWith Assets.liquidCourage (setPlayerCardOwner self.id)
    self `loadDeckCards` [liquidCourageCard]
    dirtyDeeds <- genCard Events.dirtyDeeds3
    addToHand self dirtyDeeds
    gainResources self 5

    duringTurn self do
      playCard self dirtyDeeds
      chooseTarget (toCard liquidCourageCard)
      useReactionOf shrewdDealings
      chooseTarget roland

      liquidCourage <- selectJust $ assetIs Assets.liquidCourage
      liquidCourage.controller `shouldReturn` Just (toId roland)
      liquidCourage.owner `shouldReturn` Just (toId self)

module Arkham.Asset.Assets.TheoPetersJackOfAllTrades (theoPetersJackOfAllTrades) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers

newtype TheoPetersJackOfAllTrades = TheoPetersJackOfAllTrades AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theoPetersJackOfAllTrades :: AssetCard TheoPetersJackOfAllTrades
theoPetersJackOfAllTrades = asset TheoPetersJackOfAllTrades Cards.theoPetersJackOfAllTrades

instance HasAbilities TheoPetersJackOfAllTrades where
  getAbilities (TheoPetersJackOfAllTrades a) =
    let n = toResultDefault 1 a.meta
     in [groupLimit PerGame $ restricted a 1 OnSameLocation $ parleyAction (HandDiscardCost n #any)]

instance RunMessage TheoPetersJackOfAllTrades where
  runMessage msg a@(TheoPetersJackOfAllTrades attrs) = runQueueT $ case msg of
    PlaceAsset aid _ | aid == toId attrs -> do
      n :: Int <-
        getCampaignDay <&> \case
          Day1 -> 1
          Day2 -> 2
          Day3 -> 3
      TheoPetersJackOfAllTrades <$> liftRunMessage msg (attrs & setMeta n)
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid (attrs.ability 1) 8
      pure a
    _ -> TheoPetersJackOfAllTrades <$> liftRunMessage msg attrs

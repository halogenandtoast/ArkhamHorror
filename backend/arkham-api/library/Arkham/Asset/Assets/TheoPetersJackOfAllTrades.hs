module Arkham.Asset.Assets.TheoPetersJackOfAllTrades (theoPetersJackOfAllTrades) where

import Arkham.Ability
import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Card
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (Day (..), Resident (..), codex, decreaseRelationshipLevel, getCampaignDay)

newtype TheoPetersJackOfAllTrades = TheoPetersJackOfAllTrades AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theoPetersJackOfAllTrades :: AssetCard TheoPetersJackOfAllTrades
theoPetersJackOfAllTrades = assetWith TheoPetersJackOfAllTrades Cards.theoPetersJackOfAllTrades $ (healthL ?~ 2) . (sanityL ?~ 3)

instance HasModifiersFor TheoPetersJackOfAllTrades where
  getModifiersFor (TheoPetersJackOfAllTrades a) = controllerGets a [GiveAdditionalAction $ AdditionalAction "Theo Peters" (toSource a) $ ActionRestrictedAdditionalAction #move]

instance HasAbilities TheoPetersJackOfAllTrades where
  getAbilities (TheoPetersJackOfAllTrades a) =
    let n = toResultDefault 1 a.meta
     in [ mkAbility a 99 $ forced $ AssetDefeated #when ByAny (be a)
        , groupLimit PerGame $ restricted a 1 OnSameLocation $ parleyAction (HandDiscardCost n #any)]

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
    UseCardAbility _ (isSource attrs -> True) 99 ws _ -> do
      cancelWindowBatch ws
      removeFromGame attrs
      push $ SetAsideCards [toCard attrs]
      decreaseRelationshipLevel TheoPeters 1
      pure a
    _ -> TheoPetersJackOfAllTrades <$> liftRunMessage msg attrs

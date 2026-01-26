module Arkham.Asset.Assets.JudithParkTheMuscle (judithParkTheMuscle) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers

newtype JudithParkTheMuscle = JudithParkTheMuscle AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

judithParkTheMuscle :: AssetCard JudithParkTheMuscle
judithParkTheMuscle = asset JudithParkTheMuscle Cards.judithParkTheMuscle

instance HasAbilities JudithParkTheMuscle where
  getAbilities (JudithParkTheMuscle a) =
    let n = toResultDefault 1 a.meta
     in [groupLimit PerGame $ restricted a 1 OnSameLocation $ parleyAction (ResourceCost n)]

instance RunMessage JudithParkTheMuscle where
  runMessage msg a@(JudithParkTheMuscle attrs) = runQueueT $ case msg of
    PlaceAsset aid _ | aid == toId attrs -> do
      n :: Int <-
        getCampaignDay <&> \case
          Day1 -> 1
          Day2 -> 2
          Day3 -> 3
      JudithParkTheMuscle <$> liftRunMessage msg (attrs & setMeta n)
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid (attrs.ability 1) 7
      pure a
    _ -> JudithParkTheMuscle <$> liftRunMessage msg attrs

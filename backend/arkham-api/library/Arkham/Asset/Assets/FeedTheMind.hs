module Arkham.Asset.Assets.FeedTheMind (feedTheMind) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses

newtype FeedTheMind = FeedTheMind AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

feedTheMind :: AssetCard FeedTheMind
feedTheMind = asset FeedTheMind Cards.feedTheMind

instance HasAbilities FeedTheMind where
  getAbilities (FeedTheMind a) =
    [ skillTestAbility
        $ restricted a 1 ControlsThis
        $ actionAbilityWithCost (exhaust a <> assetUseCost a Secret 1)
    ]

instance RunMessage FeedTheMind where
  runMessage msg a@(FeedTheMind attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 1)
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) (min 3 -> n) -> do
      drawCards iid (attrs.ability 1) n
      pure a
    _ -> FeedTheMind <$> liftRunMessage msg attrs

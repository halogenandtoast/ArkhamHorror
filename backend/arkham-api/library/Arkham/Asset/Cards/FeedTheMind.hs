module Arkham.Asset.Cards.FeedTheMind (feedTheMind, FeedTheMind (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype FeedTheMind = FeedTheMind AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

feedTheMind :: AssetCard FeedTheMind
feedTheMind = asset FeedTheMind Cards.feedTheMind

instance HasAbilities FeedTheMind where
  getAbilities (FeedTheMind a) =
    [ skillTestAbility
        $ restrictedAbility a 1 ControlsThis
        $ actionAbilityWithCost
        $ exhaust a
        <> assetUseCost a Secret 1
    ]

instance RunMessage FeedTheMind where
  runMessage msg a@(FeedTheMind attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 1)
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) (min 3 -> n) -> do
      push $ drawCards iid (attrs.ability 1) n
      pure a
    _ -> FeedTheMind <$> runMessage msg attrs

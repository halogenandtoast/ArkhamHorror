module Arkham.Asset.Cards.FeedTheMind (
  feedTheMind,
  FeedTheMind (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype FeedTheMind = FeedTheMind AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

feedTheMind :: AssetCard FeedTheMind
feedTheMind = asset FeedTheMind Cards.feedTheMind

instance HasAbilities FeedTheMind where
  getAbilities (FeedTheMind a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility []
        $ ActionCost 1
        <> exhaust a
        <> assetUseCost a Secret 1
    ]

instance RunMessage FeedTheMind where
  runMessage msg a@(FeedTheMind attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ beginSkillTest iid (attrs.ability 1) iid #intellect 1
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) (min 3 -> n) -> do
      pushM $ drawCards iid (toAbilitySource attrs 1) n
      pure a
    _ -> FeedTheMind <$> runMessage msg attrs

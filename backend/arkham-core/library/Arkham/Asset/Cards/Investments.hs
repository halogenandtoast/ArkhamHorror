module Arkham.Asset.Cards.Investments (
  investments,
  Investments (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype Investments = Investments AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

investments :: AssetCard Investments
investments = asset Investments Cards.investments

instance HasAbilities Investments where
  getAbilities (Investments a) =
    [ restrictedAbility a 1 (ControlsThis <> firstRestriction)
        $ FastAbility
        $ ExhaustCost (toTarget a)
    , restrictedAbility a 2 (ControlsThis <> secondRestriction)
        $ ActionAbility []
        $ ActionCost 1
        <> ExhaustCost (toTarget a)
        <> DiscardCost FromPlay (toTarget a)
    ]
   where
    firstRestriction =
      if findWithDefault 0 Supply (assetUses a) == 10 then Never else NoRestriction
    secondRestriction =
      if findWithDefault 0 Supply (assetUses a) == 0 then Never else NoRestriction

instance RunMessage Investments where
  runMessage msg a@(Investments attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      unless (findWithDefault 0 Supply (assetUses attrs) == 10)
        $ push
        $ AddUses (toId attrs) Supply 1
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      let n = findWithDefault 0 Supply (assetUses attrs)
      push $ TakeResources iid n (toSource attrs) False
      pure a
    _ -> Investments <$> runMessage msg attrs

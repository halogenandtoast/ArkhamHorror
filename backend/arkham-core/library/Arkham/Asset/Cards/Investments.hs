module Arkham.Asset.Cards.Investments
  ( investments
  , Investments(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria

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
      $ ActionAbility Nothing
      $ ActionCost 1
      <> ExhaustCost (toTarget a)
      <> DiscardCost FromPlay (toTarget a)
    ]
   where
    firstRestriction =
      if useCount (assetUses a) == 10 then Never else NoRestriction
    secondRestriction =
      if useCount (assetUses a) == 0 then Never else NoRestriction

instance RunMessage Investments where
  runMessage msg a@(Investments attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      unless (useCount (assetUses attrs) == 10) $ push $ AddUses
        (toId attrs)
        Supply
        1
      pure a
    InDiscard _ (UseCardAbility iid (isSource attrs -> True) 2 _ _) -> do
      let n = useCount (assetUses attrs)
      push $ TakeResources iid n (toSource attrs) False
      pure a
    _ -> Investments <$> runMessage msg attrs

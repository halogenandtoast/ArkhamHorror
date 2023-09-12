module Arkham.Asset.Cards.MagnifyingGlass where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype MagnifyingGlass = MagnifyingGlass AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

magnifyingGlass :: AssetCard MagnifyingGlass
magnifyingGlass = asset MagnifyingGlass Cards.magnifyingGlass

instance HasModifiersFor MagnifyingGlass where
  getModifiersFor (InvestigatorTarget iid) (MagnifyingGlass a) | controlledBy a iid = do
    pure $ toModifiers a [ActionSkillModifier Action.Investigate #intellect 1]
  getModifiersFor _ _ = pure []

instance RunMessage MagnifyingGlass where
  runMessage msg (MagnifyingGlass attrs) = MagnifyingGlass <$> runMessage msg attrs

module Arkham.Asset.Assets.Bloodstone (bloodstone) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (controllerGets)
import Arkham.Modifier

newtype Bloodstone = Bloodstone AssetAttrs
  deriving anyclass (IsAsset, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodstone :: AssetCard Bloodstone
bloodstone = assetWith Bloodstone Cards.bloodstone (healthL ?~ 2)

instance HasModifiersFor Bloodstone where
  getModifiersFor (Bloodstone a) = controllerGets a [SkillModifier #willpower 1]

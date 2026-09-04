module Arkham.Asset.Assets.Ascetic (ascetic) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers

newtype Ascetic = Ascetic AssetAttrs
  deriving anyclass (IsAsset, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascetic :: AssetCard Ascetic
ascetic = asset Ascetic Cards.ascetic

instance HasModifiersFor Ascetic where
  getModifiersFor (Ascetic a) = controllerGetsWith a setActiveDuringSetup [CannotGainXP]

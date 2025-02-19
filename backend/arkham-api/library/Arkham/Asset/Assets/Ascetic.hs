module Arkham.Asset.Assets.Ascetic (ascetic) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Modifier

newtype Ascetic = Ascetic AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascetic :: AssetCard Ascetic
ascetic = asset Ascetic Cards.ascetic

instance HasModifiersFor Ascetic where
  getModifiersFor (Ascetic a) = controllerGetsWith a setActiveDuringSetup [CannotGainXP]

instance RunMessage Ascetic where
  runMessage msg (Ascetic attrs) = runQueueT $ case msg of
    _ -> Ascetic <$> liftRunMessage msg attrs

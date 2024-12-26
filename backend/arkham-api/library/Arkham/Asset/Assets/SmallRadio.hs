module Arkham.Asset.Assets.SmallRadio (smallRadio) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype SmallRadio = SmallRadio AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smallRadio :: AssetCard SmallRadio
smallRadio = asset SmallRadio Cards.smallRadio

instance RunMessage SmallRadio where
  runMessage msg (SmallRadio attrs) = runQueueT $ case msg of
    _ -> SmallRadio <$> liftRunMessage msg attrs

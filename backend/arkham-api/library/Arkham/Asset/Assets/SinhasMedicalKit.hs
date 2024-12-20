module Arkham.Asset.Assets.SinhasMedicalKit (sinhasMedicalKit) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype SinhasMedicalKit = SinhasMedicalKit AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sinhasMedicalKit :: AssetCard SinhasMedicalKit
sinhasMedicalKit = asset SinhasMedicalKit Cards.sinhasMedicalKit

instance RunMessage SinhasMedicalKit where
  runMessage msg (SinhasMedicalKit attrs) = runQueueT $ case msg of
    _ -> SinhasMedicalKit <$> liftRunMessage msg attrs

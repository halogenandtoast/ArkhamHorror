module Arkham.Asset.Assets.ThorneConsummateProfessional (thorneConsummateProfessional) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ThorneConsummateProfessional = ThorneConsummateProfessional AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thorneConsummateProfessional :: AssetCard ThorneConsummateProfessional
thorneConsummateProfessional = asset ThorneConsummateProfessional Cards.thorneConsummateProfessional

instance RunMessage ThorneConsummateProfessional where
  runMessage msg (ThorneConsummateProfessional attrs) = runQueueT $ case msg of
    _ -> ThorneConsummateProfessional <$> liftRunMessage msg attrs

module Arkham.Asset.Assets.EceSahinTheVermillionVeiledLady (eceSahinTheVermillionVeiledLady) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype EceSahinTheVermillionVeiledLady = EceSahinTheVermillionVeiledLady AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eceSahinTheVermillionVeiledLady :: AssetCard EceSahinTheVermillionVeiledLady
eceSahinTheVermillionVeiledLady = asset EceSahinTheVermillionVeiledLady Cards.eceSahinTheVermillionVeiledLady

instance RunMessage EceSahinTheVermillionVeiledLady where
  runMessage msg (EceSahinTheVermillionVeiledLady attrs) = runQueueT $ case msg of
    _ -> EceSahinTheVermillionVeiledLady <$> liftRunMessage msg attrs

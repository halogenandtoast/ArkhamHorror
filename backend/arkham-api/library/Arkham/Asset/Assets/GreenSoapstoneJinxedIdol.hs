module Arkham.Asset.Assets.GreenSoapstoneJinxedIdol (greenSoapstoneJinxedIdol) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype GreenSoapstoneJinxedIdol = GreenSoapstoneJinxedIdol AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greenSoapstoneJinxedIdol :: AssetCard GreenSoapstoneJinxedIdol
greenSoapstoneJinxedIdol = asset GreenSoapstoneJinxedIdol Cards.greenSoapstoneJinxedIdol

instance RunMessage GreenSoapstoneJinxedIdol where
  runMessage msg (GreenSoapstoneJinxedIdol attrs) = runQueueT $ case msg of
    _ -> GreenSoapstoneJinxedIdol <$> liftRunMessage msg attrs

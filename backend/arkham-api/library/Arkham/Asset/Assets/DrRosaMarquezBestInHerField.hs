module Arkham.Asset.Assets.DrRosaMarquezBestInHerField (drRosaMarquezBestInHerField) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype DrRosaMarquezBestInHerField = DrRosaMarquezBestInHerField AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drRosaMarquezBestInHerField :: AssetCard DrRosaMarquezBestInHerField
drRosaMarquezBestInHerField = asset DrRosaMarquezBestInHerField Cards.drRosaMarquezBestInHerField

instance RunMessage DrRosaMarquezBestInHerField where
  runMessage msg (DrRosaMarquezBestInHerField attrs) = runQueueT $ case msg of
    _ -> DrRosaMarquezBestInHerField <$> liftRunMessage msg attrs

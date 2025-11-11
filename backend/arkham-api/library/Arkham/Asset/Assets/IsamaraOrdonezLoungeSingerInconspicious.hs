module Arkham.Asset.Assets.IsamaraOrdonezLoungeSingerInconspicious (isamaraOrdonezLoungeSingerInconspicious) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype IsamaraOrdonezLoungeSingerInconspicious = IsamaraOrdonezLoungeSingerInconspicious AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

isamaraOrdonezLoungeSingerInconspicious :: AssetCard IsamaraOrdonezLoungeSingerInconspicious
isamaraOrdonezLoungeSingerInconspicious = asset IsamaraOrdonezLoungeSingerInconspicious Cards.isamaraOrdonezLoungeSingerInconspicious

instance RunMessage IsamaraOrdonezLoungeSingerInconspicious where
  runMessage msg (IsamaraOrdonezLoungeSingerInconspicious attrs) = runQueueT $ case msg of
    _ -> IsamaraOrdonezLoungeSingerInconspicious <$> liftRunMessage msg attrs

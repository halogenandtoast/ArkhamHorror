module Arkham.Asset.Assets.IsamaraOrdonezLoungeSingerCrew (isamaraOrdonezLoungeSingerCrew) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype IsamaraOrdonezLoungeSingerCrew = IsamaraOrdonezLoungeSingerCrew AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

isamaraOrdonezLoungeSingerCrew :: AssetCard IsamaraOrdonezLoungeSingerCrew
isamaraOrdonezLoungeSingerCrew = asset IsamaraOrdonezLoungeSingerCrew Cards.isamaraOrdonezLoungeSingerCrew

instance RunMessage IsamaraOrdonezLoungeSingerCrew where
  runMessage msg (IsamaraOrdonezLoungeSingerCrew attrs) = runQueueT $ case msg of
    _ -> IsamaraOrdonezLoungeSingerCrew <$> liftRunMessage msg attrs

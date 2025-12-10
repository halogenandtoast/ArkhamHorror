module Arkham.Asset.Assets.AlikiZoniUperetriaTheMaidWithTheScarletSash (alikiZoniUperetriaTheMaidWithTheScarletSash) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype AlikiZoniUperetriaTheMaidWithTheScarletSash = AlikiZoniUperetriaTheMaidWithTheScarletSash AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alikiZoniUperetriaTheMaidWithTheScarletSash :: AssetCard AlikiZoniUperetriaTheMaidWithTheScarletSash
alikiZoniUperetriaTheMaidWithTheScarletSash = asset AlikiZoniUperetriaTheMaidWithTheScarletSash Cards.alikiZoniUperetriaTheMaidWithTheScarletSash

instance RunMessage AlikiZoniUperetriaTheMaidWithTheScarletSash where
  runMessage msg (AlikiZoniUperetriaTheMaidWithTheScarletSash attrs) = runQueueT $ case msg of
    _ -> AlikiZoniUperetriaTheMaidWithTheScarletSash <$> liftRunMessage msg attrs

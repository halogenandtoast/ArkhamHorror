module Arkham.Types.Asset.Cards.LeatherCoat where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes

newtype LeatherCoat = LeatherCoat AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

leatherCoat :: AssetCard LeatherCoat
leatherCoat = bodyWith LeatherCoat Cards.leatherCoat (healthL ?~ 2)

instance HasModifiersFor env LeatherCoat
instance HasActions LeatherCoat

instance (AssetRunner env) => RunMessage env LeatherCoat where
  runMessage msg (LeatherCoat attrs) = LeatherCoat <$> runMessage msg attrs

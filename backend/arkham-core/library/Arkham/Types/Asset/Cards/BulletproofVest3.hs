module Arkham.Types.Asset.Cards.BulletproofVest3 where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes

newtype BulletproofVest3 = BulletproofVest3 AssetAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

bulletproofVest3 :: AssetCard BulletproofVest3
bulletproofVest3 =
  bodyWith BulletproofVest3 Cards.bulletproofVest3 (healthL ?~ 4)

instance HasModifiersFor env BulletproofVest3

instance HasAbilities BulletproofVest3

instance (AssetRunner env) => RunMessage env BulletproofVest3 where
  runMessage msg (BulletproofVest3 attrs) =
    BulletproofVest3 <$> runMessage msg attrs

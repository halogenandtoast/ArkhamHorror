module Arkham.Asset.Cards.DejaVu5 (dejaVu5, DejaVu5 (..)) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype DejaVu5 = DejaVu5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dejaVu5 :: AssetCard DejaVu5
dejaVu5 = asset DejaVu5 Cards.dejaVu5

instance RunMessage DejaVu5 where
  runMessage msg (DejaVu5 attrs) = DejaVu5 <$> runMessage msg attrs

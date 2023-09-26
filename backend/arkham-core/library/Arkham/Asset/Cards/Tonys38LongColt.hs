module Arkham.Asset.Cards.Tonys38LongColt
  ( tonys38LongColt
  , Tonys38LongColt(..)
  )
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype Tonys38LongColt = Tonys38LongColt AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tonys38LongColt :: AssetCard Tonys38LongColt
tonys38LongColt =
  asset Tonys38LongColt Cards.tonys38LongColt

instance RunMessage Tonys38LongColt where
  runMessage msg (Tonys38LongColt attrs) = Tonys38LongColt <$> runMessage msg attrs

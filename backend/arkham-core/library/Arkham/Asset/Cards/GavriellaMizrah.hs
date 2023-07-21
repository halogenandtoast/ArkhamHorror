module Arkham.Asset.Cards.GavriellaMizrah (
  gavriellaMizrah,
  GavriellaMizrah (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype GavriellaMizrah = GavriellaMizrah AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gavriellaMizrah :: AssetCard GavriellaMizrah
gavriellaMizrah =
  allyWith GavriellaMizrah Cards.gavriellaMizrah (4, 1) (isStoryL .~ True)

instance RunMessage GavriellaMizrah where
  runMessage msg (GavriellaMizrah attrs) = GavriellaMizrah <$> runMessage msg attrs

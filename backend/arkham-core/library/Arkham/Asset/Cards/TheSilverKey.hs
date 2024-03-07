module Arkham.Asset.Cards.TheSilverKey
  ( theSilverKey
  , TheSilverKey(..)
  )
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype TheSilverKey = TheSilverKey AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSilverKey :: AssetCard TheSilverKey
theSilverKey =
  asset TheSilverKey Cards.theSilverKey

instance RunMessage TheSilverKey where
  runMessage msg (TheSilverKey attrs) = TheSilverKey <$> runMessage msg attrs

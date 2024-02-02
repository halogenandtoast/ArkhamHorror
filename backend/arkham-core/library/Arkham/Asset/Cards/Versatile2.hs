module Arkham.Asset.Cards.Versatile2
  ( versatile2
  , Versatile2(..)
  )
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype Versatile2 = Versatile2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

versatile2 :: AssetCard Versatile2
versatile2 =
  asset Versatile2 Cards.versatile2

instance RunMessage Versatile2 where
  runMessage msg (Versatile2 attrs) = Versatile2 <$> runMessage msg attrs

module Arkham.Asset.Cards.HenryDeveau
  ( henryDeveau
  , HenryDeveau(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Asset.Runner

newtype HenryDeveau = HenryDeveau AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

henryDeveau :: AssetCard HenryDeveau
henryDeveau =
  asset HenryDeveau Cards.henryDeveau

instance RunMessage HenryDeveau where
  runMessage msg (HenryDeveau attrs) = HenryDeveau <$> runMessage msg attrs

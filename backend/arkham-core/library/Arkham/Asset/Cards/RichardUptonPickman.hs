module Arkham.Asset.Cards.RichardUptonPickman
  ( richardUptonPickman
  , RichardUptonPickman(..)
  )
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype RichardUptonPickman = RichardUptonPickman AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

richardUptonPickman :: AssetCard RichardUptonPickman
richardUptonPickman =
  asset RichardUptonPickman Cards.richardUptonPickman

instance RunMessage RichardUptonPickman where
  runMessage msg (RichardUptonPickman attrs) = RichardUptonPickman <$> runMessage msg attrs

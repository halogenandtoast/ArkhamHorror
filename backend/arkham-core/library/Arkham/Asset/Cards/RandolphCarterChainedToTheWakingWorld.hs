module Arkham.Asset.Cards.RandolphCarterChainedToTheWakingWorld
  ( randolphCarterChainedToTheWakingWorld
  , RandolphCarterChainedToTheWakingWorld(..)
  )
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype RandolphCarterChainedToTheWakingWorld = RandolphCarterChainedToTheWakingWorld AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

randolphCarterChainedToTheWakingWorld :: AssetCard RandolphCarterChainedToTheWakingWorld
randolphCarterChainedToTheWakingWorld =
  asset RandolphCarterChainedToTheWakingWorld Cards.randolphCarterChainedToTheWakingWorld

instance RunMessage RandolphCarterChainedToTheWakingWorld where
  runMessage msg (RandolphCarterChainedToTheWakingWorld attrs) = RandolphCarterChainedToTheWakingWorld <$> runMessage msg attrs

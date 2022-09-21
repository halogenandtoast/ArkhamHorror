module Arkham.Asset.Cards.RelicOfAgesForestallingTheFuture
  ( relicOfAgesForestallingTheFuture
  , RelicOfAgesForestallingTheFuture(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype RelicOfAgesForestallingTheFuture = RelicOfAgesForestallingTheFuture AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relicOfAgesForestallingTheFuture :: AssetCard RelicOfAgesForestallingTheFuture
relicOfAgesForestallingTheFuture =
  asset RelicOfAgesForestallingTheFuture Cards.relicOfAgesForestallingTheFuture

instance RunMessage RelicOfAgesForestallingTheFuture where
  runMessage msg (RelicOfAgesForestallingTheFuture attrs) =
    RelicOfAgesForestallingTheFuture <$> runMessage msg attrs

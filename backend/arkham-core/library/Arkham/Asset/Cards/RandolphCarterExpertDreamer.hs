module Arkham.Asset.Cards.RandolphCarterExpertDreamer (
  randolphCarterExpertDreamer,
  RandolphCarterExpertDreamer (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype RandolphCarterExpertDreamer = RandolphCarterExpertDreamer AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

randolphCarterExpertDreamer :: AssetCard RandolphCarterExpertDreamer
randolphCarterExpertDreamer = ally RandolphCarterExpertDreamer Cards.randolphCarterExpertDreamer (3, 2)

instance RunMessage RandolphCarterExpertDreamer where
  runMessage msg (RandolphCarterExpertDreamer attrs) = RandolphCarterExpertDreamer <$> runMessage msg attrs

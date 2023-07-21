module Arkham.Asset.Cards.PennyWhite (
  pennyWhite,
  PennyWhite (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype PennyWhite = PennyWhite AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pennyWhite :: AssetCard PennyWhite
pennyWhite =
  allyWith PennyWhite Cards.pennyWhite (3, 2) (isStoryL .~ True)

instance RunMessage PennyWhite where
  runMessage msg (PennyWhite attrs) = PennyWhite <$> runMessage msg attrs

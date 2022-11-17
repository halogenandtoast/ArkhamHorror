module Arkham.Asset.Cards.RelicOfAgesRepossessThePast
  ( relicOfAgesRepossessThePast
  , RelicOfAgesRepossessThePast(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype RelicOfAgesRepossessThePast = RelicOfAgesRepossessThePast AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relicOfAgesRepossessThePast :: AssetCard RelicOfAgesRepossessThePast
relicOfAgesRepossessThePast =
  asset RelicOfAgesRepossessThePast Cards.relicOfAgesRepossessThePast

instance RunMessage RelicOfAgesRepossessThePast where
  runMessage msg (RelicOfAgesRepossessThePast attrs) =
    RelicOfAgesRepossessThePast <$> runMessage msg attrs

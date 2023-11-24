module Arkham.Asset.Cards.DrShivaniMaheswaran
  ( drShivaniMaheswaran
  , DrShivaniMaheswaran(..)
  )
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype DrShivaniMaheswaran = DrShivaniMaheswaran AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drShivaniMaheswaran :: AssetCard DrShivaniMaheswaran
drShivaniMaheswaran =
  asset DrShivaniMaheswaran Cards.drShivaniMaheswaran

instance RunMessage DrShivaniMaheswaran where
  runMessage msg (DrShivaniMaheswaran attrs) = DrShivaniMaheswaran <$> runMessage msg attrs

module Arkham.Asset.Cards.ManagersKey (
  managersKey,
  ManagersKey (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype ManagersKey = ManagersKey AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

managersKey :: AssetCard ManagersKey
managersKey = asset ManagersKey Cards.managersKey

instance RunMessage ManagersKey where
  runMessage msg a@(ManagersKey attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ putCardIntoPlay iid attrs
      pure a
    _ -> ManagersKey <$> runMessage msg attrs

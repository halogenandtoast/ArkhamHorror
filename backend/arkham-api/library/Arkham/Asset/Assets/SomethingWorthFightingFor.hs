module Arkham.Asset.Assets.SomethingWorthFightingFor (
  somethingWorthFightingFor,
  SomethingWorthFightingFor (..),
) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype SomethingWorthFightingFor = SomethingWorthFightingFor AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

somethingWorthFightingFor :: AssetCard SomethingWorthFightingFor
somethingWorthFightingFor =
  assetWith
    SomethingWorthFightingFor
    Cards.somethingWorthFightingFor
    (sanityL ?~ 3)

instance HasModifiersFor SomethingWorthFightingFor where
  getModifiersFor (SomethingWorthFightingFor a) = case a.controller of
    Nothing -> pure mempty
    Just iid ->
      modifySelect
        a
        (not_ (InvestigatorWithId iid) <> at_ (locationWithAsset a))
        [CanAssignHorrorToAsset a.id]

instance RunMessage SomethingWorthFightingFor where
  runMessage msg (SomethingWorthFightingFor attrs) =
    SomethingWorthFightingFor <$> runMessage msg attrs

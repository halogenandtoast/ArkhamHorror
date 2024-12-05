module Arkham.Asset.Assets.SchoffnersCatalogue (
  schoffnersCatalogue,
  SchoffnersCatalogue (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype SchoffnersCatalogue = SchoffnersCatalogue AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

schoffnersCatalogue :: AssetCard SchoffnersCatalogue
schoffnersCatalogue = assetWith SchoffnersCatalogue Cards.schoffnersCatalogue discardWhenNoUses

instance HasModifiersFor SchoffnersCatalogue where
  getModifiersFor (SchoffnersCatalogue a) = case a.controller of
    Nothing -> pure mempty
    Just iid ->
      controllerGets
        a
        [CanSpendUsesAsResourceOnCardFromInvestigator a.id Secret (colocatedWith iid) (#item <> #asset)]

instance RunMessage SchoffnersCatalogue where
  runMessage msg (SchoffnersCatalogue attrs) = SchoffnersCatalogue <$> runMessage msg attrs

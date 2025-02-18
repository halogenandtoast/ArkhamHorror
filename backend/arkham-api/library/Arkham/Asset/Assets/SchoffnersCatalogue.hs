module Arkham.Asset.Assets.SchoffnersCatalogue (schoffnersCatalogue) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude

newtype SchoffnersCatalogue = SchoffnersCatalogue AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

schoffnersCatalogue :: AssetCard SchoffnersCatalogue
schoffnersCatalogue = assetWith SchoffnersCatalogue Cards.schoffnersCatalogue discardWhenNoUses

instance HasModifiersFor SchoffnersCatalogue where
  getModifiersFor (SchoffnersCatalogue a) = for_ a.controller \iid -> do
    controllerGets
      a
      [CanSpendUsesAsResourceOnCardFromInvestigator a.id Secret (colocatedWith iid) (#item <> #asset)]

instance RunMessage SchoffnersCatalogue where
  runMessage msg (SchoffnersCatalogue attrs) = SchoffnersCatalogue <$> runMessage msg attrs

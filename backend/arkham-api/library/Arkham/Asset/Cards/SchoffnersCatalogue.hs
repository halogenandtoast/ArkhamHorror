module Arkham.Asset.Cards.SchoffnersCatalogue (
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
  getModifiersFor (InvestigatorTarget iid) (SchoffnersCatalogue attrs) =
    pure
      $ toModifiers
        attrs
        [ CanSpendUsesAsResourceOnCardFromInvestigator
          (toId attrs)
          Secret
          (colocatedWith iid)
          (#item <> #asset)
        | attrs `controlledBy` iid
        ]
  getModifiersFor _ _ = pure []

instance RunMessage SchoffnersCatalogue where
  runMessage msg (SchoffnersCatalogue attrs) = SchoffnersCatalogue <$> runMessage msg attrs

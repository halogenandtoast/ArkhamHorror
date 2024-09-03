module Arkham.Asset.Cards.SomethingWorthFightingFor (
  somethingWorthFightingFor,
  SomethingWorthFightingFor (..),
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

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
  getModifiersFor (InvestigatorTarget iid) (SomethingWorthFightingFor a)
    | not (controlledBy a iid) = do
        locationId <- field InvestigatorLocation iid
        assetLocationId <- field AssetLocation (toId a)
        pure
          [ toModifier a (CanAssignHorrorToAsset $ toId a)
          | (locationId == assetLocationId) && isJust locationId
          ]
  getModifiersFor _ _ = pure []

instance RunMessage SomethingWorthFightingFor where
  runMessage msg (SomethingWorthFightingFor attrs) =
    SomethingWorthFightingFor <$> runMessage msg attrs

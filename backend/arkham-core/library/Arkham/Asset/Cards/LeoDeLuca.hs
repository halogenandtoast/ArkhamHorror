module Arkham.Asset.Cards.LeoDeLuca where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype LeoDeLuca = LeoDeLuca AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

leoDeLuca :: AssetCard LeoDeLuca
leoDeLuca = ally LeoDeLuca Cards.leoDeLuca (2, 2)

instance HasModifiersFor LeoDeLuca where
  getModifiersFor (InvestigatorTarget iid) (LeoDeLuca a) = do
    pure
      $ toModifiers a [AdditionalActions "Leo De Luca" (toSource a) 1 | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance RunMessage LeoDeLuca where
  runMessage msg (LeoDeLuca attrs) = LeoDeLuca <$> runMessage msg attrs

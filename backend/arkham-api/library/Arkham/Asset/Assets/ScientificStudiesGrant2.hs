module Arkham.Asset.Assets.ScientificStudiesGrant2 (scientificStudiesGrant2) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Script
import Arkham.Trait

newtype ScientificStudiesGrant2 = ScientificStudiesGrant2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scientificStudiesGrant2 :: AssetCard ScientificStudiesGrant2
scientificStudiesGrant2 = asset ScientificStudiesGrant2 Cards.scientificStudiesGrant2

instance RunMessage ScientificStudiesGrant2 where
  runMessage = script $ additionalSlots #hand 2 $ holds [Science, Tool]

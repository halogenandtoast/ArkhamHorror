module Arkham.Asset.Cards.LeoDeLuca1 where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Modifier

newtype LeoDeLuca1 = LeoDeLuca1 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leoDeLuca1 :: AssetCard LeoDeLuca1
leoDeLuca1 = ally LeoDeLuca1 Cards.leoDeLuca1 (2, 2)

instance HasModifiersFor LeoDeLuca1 where
  getModifiersFor (InvestigatorTarget iid) (LeoDeLuca1 a) =
    modified a [AdditionalActions "Leo de Luca" (toSource a) 1 | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance RunMessage LeoDeLuca1 where
  runMessage msg (LeoDeLuca1 attrs) = LeoDeLuca1 <$> runMessage msg attrs

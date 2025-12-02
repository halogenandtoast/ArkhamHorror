module Arkham.Asset.Assets.KeyLocusLastBastion (keyLocusLastBastion) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (modifySelf)
import Arkham.Scenarios.DogsOfWar.Helpers (pattern IsKeyLocus)

newtype KeyLocusLastBastion = KeyLocusLastBastion AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keyLocusLastBastion :: AssetCard KeyLocusLastBastion
keyLocusLastBastion = assetWith KeyLocusLastBastion Cards.keyLocusLastBastion (healthL ?~ 6)

instance HasModifiersFor KeyLocusLastBastion where
  getModifiersFor (KeyLocusLastBastion a) = modifySelf a [IsKeyLocus]

instance RunMessage KeyLocusLastBastion where
  runMessage msg (KeyLocusLastBastion attrs) = runQueueT $ case msg of
    _ -> KeyLocusLastBastion <$> liftRunMessage msg attrs

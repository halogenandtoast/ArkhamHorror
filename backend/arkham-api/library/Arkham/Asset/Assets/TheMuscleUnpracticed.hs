module Arkham.Asset.Assets.TheMuscleUnpracticed (theMuscleUnpracticed) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype TheMuscleUnpracticed = TheMuscleUnpracticed AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMuscleUnpracticed :: AssetCard TheMuscleUnpracticed
theMuscleUnpracticed = asset TheMuscleUnpracticed Cards.theMuscleUnpracticed

instance RunMessage TheMuscleUnpracticed where
  runMessage msg (TheMuscleUnpracticed attrs) = runQueueT $ case msg of
    _ -> TheMuscleUnpracticed <$> liftRunMessage msg attrs

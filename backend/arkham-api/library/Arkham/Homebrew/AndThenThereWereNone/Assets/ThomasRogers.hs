{- | Thomas Rogers, "The Butler": {combat}{wild} and no game text.

The icons are the evidence act 1b tallies; the only thing the asset does is
turn over when the verdict convicts it.
-}
module Arkham.Homebrew.AndThenThereWereNone.Assets.ThomasRogers (thomasRogers) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AndThenThereWereNone.Helpers (flipToCulprit)

newtype ThomasRogers = ThomasRogers AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thomasRogers :: AssetCard ThomasRogers
thomasRogers = asset ThomasRogers Cards.thomasRogers

instance RunMessage ThomasRogers where
  runMessage msg a@(ThomasRogers attrs) = runQueueT $ case msg of
    -- "Flip that story asset to its enemy side." (act 1b's verdict)
    Flip _ _ (isTarget attrs -> True) -> do
      flipToCulprit Enemies.thomasRogersColdAndDistant attrs
      pure a
    _ -> ThomasRogers <$> liftRunMessage msg attrs

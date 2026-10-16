{- | Edward Armstrong, "The Doctor": {intellect}{wild} and no game text.

The icons are the evidence act 1b tallies; the only thing the asset does is
turn over when the verdict convicts it.
-}
module Arkham.Homebrew.AndThenThereWereNone.Assets.EdwardArmstrong (edwardArmstrong) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AndThenThereWereNone.Helpers (flipToCulprit)

newtype EdwardArmstrong = EdwardArmstrong AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

edwardArmstrong :: AssetCard EdwardArmstrong
edwardArmstrong = asset EdwardArmstrong Cards.edwardArmstrong

instance RunMessage EdwardArmstrong where
  runMessage msg a@(EdwardArmstrong attrs) = runQueueT $ case msg of
    -- "Flip that story asset to its enemy side." (act 1b's verdict)
    Flip _ _ (isTarget attrs -> True) -> do
      flipToCulprit Enemies.edwardArmstrongDreamingDeeply attrs
      pure a
    _ -> EdwardArmstrong <$> liftRunMessage msg attrs

{- | Emily Brent, "The Spinster": {willpower}{intellect} and no game text.

The icons are the evidence act 1b tallies; the only thing the asset does is
turn over when the verdict convicts it.
-}
module Arkham.Homebrew.AndThenThereWereNone.Assets.EmilyBrent (emilyBrent) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AndThenThereWereNone.Helpers (flipToCulprit)

newtype EmilyBrent = EmilyBrent AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emilyBrent :: AssetCard EmilyBrent
emilyBrent = asset EmilyBrent Cards.emilyBrent

instance RunMessage EmilyBrent where
  runMessage msg a@(EmilyBrent attrs) = runQueueT $ case msg of
    -- "Flip that story asset to its enemy side." (act 1b's verdict)
    Flip _ _ (isTarget attrs -> True) -> do
      flipToCulprit Enemies.emilyBrentInATangledWeb attrs
      pure a
    _ -> EmilyBrent <$> liftRunMessage msg attrs

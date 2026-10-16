{- | William Blore, "The Inspector": {intellect}{combat} and no game text.

The icons are the evidence act 1b tallies; the only thing the asset does is
turn over when the verdict convicts it.
-}
module Arkham.Homebrew.AndThenThereWereNone.Assets.WilliamBlore (williamBlore) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AndThenThereWereNone.Helpers (flipToCulprit)

newtype WilliamBlore = WilliamBlore AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

williamBlore :: AssetCard WilliamBlore
williamBlore = asset WilliamBlore Cards.williamBlore

instance RunMessage WilliamBlore where
  runMessage msg a@(WilliamBlore attrs) = runQueueT $ case msg of
    -- "Flip that story asset to its enemy side." (act 1b's verdict)
    Flip _ _ (isTarget attrs -> True) -> do
      flipToCulprit Enemies.williamBloreInTheBusinessOfSecrets attrs
      pure a
    _ -> WilliamBlore <$> liftRunMessage msg attrs

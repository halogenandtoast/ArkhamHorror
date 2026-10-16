{- | Philip Lombard, "The Soldier": {combat}{agility} and no game text.

The icons are the evidence act 1b tallies; the only thing the asset does is
turn over when the verdict convicts it.
-}
module Arkham.Homebrew.AndThenThereWereNone.Assets.PhilipLombard (philipLombard) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AndThenThereWereNone.Helpers (flipToCulprit)

newtype PhilipLombard = PhilipLombard AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

philipLombard :: AssetCard PhilipLombard
philipLombard = asset PhilipLombard Cards.philipLombard

instance RunMessage PhilipLombard where
  runMessage msg a@(PhilipLombard attrs) = runQueueT $ case msg of
    -- "Flip that story asset to its enemy side." (act 1b's verdict)
    Flip _ _ (isTarget attrs -> True) -> do
      flipToCulprit Enemies.philipLombardSnakeInTheGrass attrs
      pure a
    _ -> PhilipLombard <$> liftRunMessage msg attrs

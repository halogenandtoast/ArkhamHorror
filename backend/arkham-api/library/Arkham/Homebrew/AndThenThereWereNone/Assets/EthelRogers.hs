{- | Ethel Rogers, "The Housekeeper": {willpower}{agility} and no game text.

The icons are the evidence act 1b tallies; the only thing the asset does is
turn over when the verdict convicts it.
-}
module Arkham.Homebrew.AndThenThereWereNone.Assets.EthelRogers (ethelRogers) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AndThenThereWereNone.Helpers (flipToCulprit)

newtype EthelRogers = EthelRogers AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ethelRogers :: AssetCard EthelRogers
ethelRogers = asset EthelRogers Cards.ethelRogers

instance RunMessage EthelRogers where
  runMessage msg a@(EthelRogers attrs) = runQueueT $ case msg of
    -- "Flip that story asset to its enemy side." (act 1b's verdict)
    Flip _ _ (isTarget attrs -> True) -> do
      flipToCulprit Enemies.ethelRogersMaidenNameMarsh attrs
      pure a
    _ -> EthelRogers <$> liftRunMessage msg attrs

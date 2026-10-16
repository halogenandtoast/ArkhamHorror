{- | Vera Claythorne, "The Governess": {intellect}{agility} and no game text.

The icons are the evidence act 1b tallies; the only thing the asset does is
turn over when the verdict convicts it.
-}
module Arkham.Homebrew.AndThenThereWereNone.Assets.VeraClaythorne (veraClaythorne) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AndThenThereWereNone.Helpers (flipToCulprit)

newtype VeraClaythorne = VeraClaythorne AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

veraClaythorne :: AssetCard VeraClaythorne
veraClaythorne = asset VeraClaythorne Cards.veraClaythorne

instance RunMessage VeraClaythorne where
  runMessage msg a@(VeraClaythorne attrs) = runQueueT $ case msg of
    -- "Flip that story asset to its enemy side." (act 1b's verdict)
    Flip _ _ (isTarget attrs -> True) -> do
      flipToCulprit Enemies.veraClaythorneDiscipliningTheYoung attrs
      pure a
    _ -> VeraClaythorne <$> liftRunMessage msg attrs

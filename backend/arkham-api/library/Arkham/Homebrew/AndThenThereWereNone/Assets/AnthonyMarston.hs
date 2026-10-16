{- | Anthony Marston, "The Playboy": {agility}{wild} and no game text.

The icons are the evidence act 1b tallies; the only thing the asset does is
turn over when the verdict convicts it.
-}
module Arkham.Homebrew.AndThenThereWereNone.Assets.AnthonyMarston (anthonyMarston) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AndThenThereWereNone.Helpers (flipToCulprit)

newtype AnthonyMarston = AnthonyMarston AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anthonyMarston :: AssetCard AnthonyMarston
anthonyMarston = asset AnthonyMarston Cards.anthonyMarston

instance RunMessage AnthonyMarston where
  runMessage msg a@(AnthonyMarston attrs) = runQueueT $ case msg of
    -- "Flip that story asset to its enemy side." (act 1b's verdict)
    Flip _ _ (isTarget attrs -> True) -> do
      flipToCulprit Enemies.anthonyMarstonDelusionsOfGrandeur attrs
      pure a
    _ -> AnthonyMarston <$> liftRunMessage msg attrs

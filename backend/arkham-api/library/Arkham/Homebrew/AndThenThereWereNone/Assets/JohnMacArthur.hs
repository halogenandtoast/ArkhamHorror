{- | John MacArthur, "The General": {willpower}{combat} and no game text.

The icons are the evidence act 1b tallies; the only thing the asset does is
turn over when the verdict convicts it.
-}
module Arkham.Homebrew.AndThenThereWereNone.Assets.JohnMacArthur (johnMacArthur) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AndThenThereWereNone.Helpers (flipToCulprit)

newtype JohnMacArthur = JohnMacArthur AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

johnMacArthur :: AssetCard JohnMacArthur
johnMacArthur = asset JohnMacArthur Cards.johnMacArthur

instance RunMessage JohnMacArthur where
  runMessage msg a@(JohnMacArthur attrs) = runQueueT $ case msg of
    -- "Flip that story asset to its enemy side." (act 1b's verdict)
    Flip _ _ (isTarget attrs -> True) -> do
      flipToCulprit Enemies.johnMacArthurAmountedToNothing attrs
      pure a
    _ -> JohnMacArthur <$> liftRunMessage msg attrs

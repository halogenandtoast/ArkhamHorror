{- | Lawrence Wargrave, "The Judge": {willpower}{wild} and no game text.

The icons are the evidence act 1b tallies; the only thing the asset does is
turn over when the verdict convicts it.
-}
module Arkham.Homebrew.AndThenThereWereNone.Assets.LawrenceWargrave (lawrenceWargrave) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AndThenThereWereNone.Helpers (flipToCulprit)

newtype LawrenceWargrave = LawrenceWargrave AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lawrenceWargrave :: AssetCard LawrenceWargrave
lawrenceWargrave = asset LawrenceWargrave Cards.lawrenceWargrave

instance RunMessage LawrenceWargrave where
  runMessage msg a@(LawrenceWargrave attrs) = runQueueT $ case msg of
    -- "Flip that story asset to its enemy side." (act 1b's verdict)
    Flip _ _ (isTarget attrs -> True) -> do
      flipToCulprit Enemies.lawrenceWargraveManOfManyFaces attrs
      pure a
    _ -> LawrenceWargrave <$> liftRunMessage msg attrs

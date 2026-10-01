module Arkham.Homebrew.CircusExMortis.Locations.FallenCopse (fallenCopse) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FallenCopse = FallenCopse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fallenCopse :: LocationCard FallenCopse
fallenCopse = location FallenCopse Cards.fallenCopse 4 (PerPlayer 2)

{- | "[free] Choose and discard a non-weakness asset you control: Replenish up to 4 clues
on Fallen Copse."

'DiscardAssetCost' checks 'DiscardableAsset' when the ability is offered, so the [free]
is never shown with nothing to discard; 'LocationNotAtClueLimit' does the same for the
other half, since replenishing cannot take a location past its printed clue value.
-}
instance HasAbilities FallenCopse where
  getAbilities (FallenCopse a) =
    extendRevealed1 a
      $ fastAbility a 1 (DiscardAssetCost $ AssetControlledBy You <> NonWeaknessAsset)
      $ Here
      <> thisExists a LocationNotAtClueLimit

instance RunMessage FallenCopse where
  runMessage msg l@(FallenCopse attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ PlaceCluesUpToClueValue attrs.id (attrs.ability 1) 4
      pure l
    _ -> FallenCopse <$> liftRunMessage msg attrs

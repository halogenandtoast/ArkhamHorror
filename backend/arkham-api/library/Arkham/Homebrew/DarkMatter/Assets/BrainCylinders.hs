module Arkham.Homebrew.DarkMatter.Assets.BrainCylinders (
  brainCylinder089,
  brainCylinder114,
  brainCylinder367,
) where

import Arkham.Asset.Import.Lifted
import Arkham.Card.CardDef (CardDef)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards

{- | Strange Moons' three [[Brain]] story assets carry no rules text — each simply
prints one scanning icon (see @Helpers.printedIcons@), which the [[Interface]]
locations pair with their own icon when scanning. They start attached to Brain
Storage and are moved between [[Interface]] locations by its action.
-}
newtype BrainCylinder = BrainCylinder AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mkBrainCylinder :: CardDef -> AssetCard BrainCylinder
mkBrainCylinder = asset BrainCylinder

brainCylinder089 :: AssetCard BrainCylinder
brainCylinder089 = mkBrainCylinder Cards.brainCylinder089

brainCylinder114 :: AssetCard BrainCylinder
brainCylinder114 = mkBrainCylinder Cards.brainCylinder114

brainCylinder367 :: AssetCard BrainCylinder
brainCylinder367 = mkBrainCylinder Cards.brainCylinder367

instance RunMessage BrainCylinder where
  runMessage msg (BrainCylinder attrs) = BrainCylinder <$> runMessage msg attrs

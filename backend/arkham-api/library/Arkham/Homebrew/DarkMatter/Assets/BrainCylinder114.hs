module Arkham.Homebrew.DarkMatter.Assets.BrainCylinder114 (brainCylinder114) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards

{- | One of Strange Moons' three [[Brain]] story assets. They carry no rules text
— each simply prints one scanning icon (see @Helpers.printedIcons@), which the
[[Interface]] locations pair with their own icon when scanning. They start
attached to Brain Storage and are moved between [[Interface]] locations by its
action.
-}
newtype BrainCylinder114 = BrainCylinder114 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brainCylinder114 :: AssetCard BrainCylinder114
brainCylinder114 = assetWith BrainCylinder114 Cards.brainCylinder114 (healthL ?~ 2)

instance RunMessage BrainCylinder114 where
  runMessage msg (BrainCylinder114 attrs) = BrainCylinder114 <$> runMessage msg attrs

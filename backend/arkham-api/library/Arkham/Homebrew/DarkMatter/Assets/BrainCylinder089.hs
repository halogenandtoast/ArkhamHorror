module Arkham.Homebrew.DarkMatter.Assets.BrainCylinder089 (brainCylinder089) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards

{- | One of Strange Moons' three [[Brain]] story assets. They carry no rules text
— each simply prints one scanning icon (see @Helpers.printedIcons@), which the
[[Interface]] locations pair with their own icon when scanning. They start
attached to Brain Storage and are moved between [[Interface]] locations by its
action.
-}
newtype BrainCylinder089 = BrainCylinder089 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brainCylinder089 :: AssetCard BrainCylinder089
brainCylinder089 = assetWith BrainCylinder089 Cards.brainCylinder089 (healthL ?~ 2)

instance RunMessage BrainCylinder089 where
  runMessage msg (BrainCylinder089 attrs) = BrainCylinder089 <$> runMessage msg attrs

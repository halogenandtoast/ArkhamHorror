module Arkham.Homebrew.DarkMatter.Assets.BrainCylinder367 (brainCylinder367) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards

{- | One of Strange Moons' three [[Brain]] story assets. They carry no rules text
— each simply prints one scanning icon (see @Helpers.printedIcons@), which the
[[Interface]] locations pair with their own icon when scanning. They start
attached to Brain Storage and are moved between [[Interface]] locations by its
action.
-}
newtype BrainCylinder367 = BrainCylinder367 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brainCylinder367 :: AssetCard BrainCylinder367
brainCylinder367 = asset BrainCylinder367 Cards.brainCylinder367

instance RunMessage BrainCylinder367 where
  runMessage msg (BrainCylinder367 attrs) = BrainCylinder367 <$> runMessage msg attrs

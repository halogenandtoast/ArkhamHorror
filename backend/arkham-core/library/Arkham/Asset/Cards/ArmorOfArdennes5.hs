module Arkham.Asset.Cards.ArmorOfArdennes5 (armorOfArdennes5, ArmorOfArdennes5 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Window (getDamageSource)
import Arkham.Matcher

newtype ArmorOfArdennes5 = ArmorOfArdennes5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

armorOfArdennes5 :: AssetCard ArmorOfArdennes5
armorOfArdennes5 = assetWith ArmorOfArdennes5 Cards.armorOfArdennes5 (healthL ?~ 4)

instance HasAbilities ArmorOfArdennes5 where
  getAbilities (ArmorOfArdennes5 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (AssetDealtDamage #when AnyCancellableSource $ be a) (exhaust a)
    ]

instance RunMessage ArmorOfArdennes5 where
  runMessage msg a@(ArmorOfArdennes5 attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getDamageSource -> dSource) _ -> do
      push $ CancelAssetDamage attrs.id dSource 1
      pure a
    _ -> ArmorOfArdennes5 <$> liftRunMessage msg attrs

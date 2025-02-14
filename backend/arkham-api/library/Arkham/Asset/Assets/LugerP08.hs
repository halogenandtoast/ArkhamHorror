module Arkham.Asset.Assets.LugerP08 (lugerP08) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetExhausted)
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Modifier

newtype LugerP08 = LugerP08 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lugerP08 :: AssetCard LugerP08
lugerP08 = asset LugerP08 Cards.lugerP08

instance HasAbilities LugerP08 where
  getAbilities (LugerP08 a) =
    [ restricted a 1 ControlsThis $ FastAbility' (exhaust a <> assetUseCost a Ammo 1) [#fight]
    , controlled a 2 (thisExists a $ oneOf [AssetExhausted, AssetNotAtUsesX])
        $ actionAbilityWithCost (ResourceCost 1)
    ]

instance RunMessage LugerP08 where
  runMessage msg a@(LugerP08 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #combat 1)
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      readyThis attrs
      pure . LugerP08 $ attrs & tokensL . at Ammo ?~ 2
    _ -> LugerP08 <$> liftRunMessage msg attrs

module Arkham.Asset.Assets.M1911 (m1911) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Modifier

newtype M1911 = M1911 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

m1911 :: AssetCard M1911
m1911 = asset M1911 Cards.m1911

instance HasAbilities M1911 where
  getAbilities (M1911 a) = [skillTestAbility $ controlled_ a 1 $ fightAction (assetUseCost a Ammo 1)]

instance RunMessage M1911 where
  runMessage msg a@(M1911 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #combat 1, DamageDealt 1]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    _ -> M1911 <$> liftRunMessage msg attrs

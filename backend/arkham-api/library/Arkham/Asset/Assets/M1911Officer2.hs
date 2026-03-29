module Arkham.Asset.Assets.M1911Officer2 (m1911Officer2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Modifier

newtype M1911Officer2 = M1911Officer2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

m1911Officer2 :: AssetCard M1911Officer2
m1911Officer2 = asset M1911Officer2 Cards.m1911Officer2

instance HasAbilities M1911Officer2 where
  getAbilities (M1911Officer2 a) = [skillTestAbility $ controlled_ a 1 $ fightAction (assetUseCost a Ammo 1)]

instance RunMessage M1911Officer2 where
  runMessage msg a@(M1911Officer2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [SkillModifier #combat 2, DamageDealt 1, IgnoreRetaliate]
      chooseFightEnemy sid iid source
      pure a
    _ -> M1911Officer2 <$> liftRunMessage msg attrs

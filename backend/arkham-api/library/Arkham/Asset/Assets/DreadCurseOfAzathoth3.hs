module Arkham.Asset.Assets.DreadCurseOfAzathoth3 (dreadCurseOfAzathoth3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher hiding (EnemyDefeated)
import Arkham.Modifier

newtype DreadCurseOfAzathoth3 = DreadCurseOfAzathoth3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreadCurseOfAzathoth3 :: AssetCard DreadCurseOfAzathoth3
dreadCurseOfAzathoth3 = asset DreadCurseOfAzathoth3 Cards.dreadCurseOfAzathoth3

instance HasAbilities DreadCurseOfAzathoth3 where
  getAbilities (DreadCurseOfAzathoth3 a) =
    [controlled_ a 1 $ fightActionWith #willpower (DoomCost (toSource a) (toTarget a) 1)]

instance RunMessage DreadCurseOfAzathoth3 where
  runMessage msg a@(DreadCurseOfAzathoth3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [SkillModifier #willpower 2, DamageDealt 1]
      onRevealChaosTokenEffect sid (oneOf [#skull, #elderthing]) source attrs do
        removeAllDoom source attrs
      chooseFightEnemyWith #willpower sid iid source
      pure a
    EnemyDefeated _ _ (isAbilitySource attrs 1 -> True) _ -> do
      when (attrs.doom > 0) $ removeDoom (attrs.ability 1) attrs 1
      pure a
    _ -> DreadCurseOfAzathoth3 <$> liftRunMessage msg attrs

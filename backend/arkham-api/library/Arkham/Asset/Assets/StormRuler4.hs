module Arkham.Asset.Assets.StormRuler4 (stormRuler4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTestResolvedChaosTokens)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype StormRuler4 = StormRuler4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stormRuler4 :: AssetCard StormRuler4
stormRuler4 = asset StormRuler4 Cards.stormRuler4

instance HasAbilities StormRuler4 where
  getAbilities (StormRuler4 a) =
    [restrictedAbility a 1 ControlsThis fightAction_]

instance RunMessage StormRuler4 where
  runMessage msg a@(StormRuler4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid attrs iid [AddSkillValue #willpower]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      let faces = [#skull, #cultist, #tablet, #elderthing]
      tokens <- min 3 . count ((`elem` faces) . (.face)) <$> getSkillTestResolvedChaosTokens
      enemies <- select $ enemyAtLocationWith iid <> EnemyCanBeDamagedBySource (toSource attrs)
      when (tokens > 0 && notNull enemies) do
        chooseOneM iid do
          labeled ("Exhaust Storm Ruler to deal " <> tshow tokens <> " damage to an enemy at your location") do
            exhaustThis attrs
            chooseTargetM iid enemies (nonAttackEnemyDamage (Just iid) attrs tokens)
          labeled "Do not exhaust" nothing
      pure a
    _ -> StormRuler4 <$> liftRunMessage msg attrs

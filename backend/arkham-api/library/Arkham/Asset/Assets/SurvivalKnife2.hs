module Arkham.Asset.Assets.SurvivalKnife2 (survivalKnife2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Window.Enemy (getEnemy)
import Arkham.Matcher
import Arkham.Modifier

newtype SurvivalKnife2 = SurvivalKnife2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

survivalKnife2 :: AssetCard SurvivalKnife2
survivalKnife2 = asset SurvivalKnife2 Cards.survivalKnife2

instance HasAbilities SurvivalKnife2 where
  getAbilities (SurvivalKnife2 a) =
    [ fightAbility a 1 mempty ControlsThis
    , controlled a 2 (DuringPhase #enemy)
        $ triggered (EnemyAttacks #when You AnyEnemyAttack AnyEnemy) (exhaust a)
    ]

instance RunMessage SurvivalKnife2 where
  runMessage msg a@(SurvivalKnife2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseFightEnemyWithModifiers sid iid (attrs.ability 1) [SkillModifier #combat 2]
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 (getEnemy -> enemy) _ -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 2) iid [SkillModifier #combat 2, DamageDealt 1]
      fightEnemy sid iid (attrs.ability 2) enemy
      pure a
    _ -> SurvivalKnife2 <$> liftRunMessage msg attrs

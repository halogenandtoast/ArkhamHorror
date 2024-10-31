module Arkham.Asset.Assets.FireExtinguisher1 where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Import
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Modifier

newtype FireExtinguisher1 = FireExtinguisher1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fireExtinguisher1 :: AssetCard FireExtinguisher1
fireExtinguisher1 = asset FireExtinguisher1 Cards.fireExtinguisher1

instance HasAbilities FireExtinguisher1 where
  getAbilities (FireExtinguisher1 a) =
    [ restricted a 1 ControlsThis fightAction_
    , restricted a 2 ControlsThis $ evadeAction (ExileCost $ toTarget a)
    ]

instance RunMessage FireExtinguisher1 where
  runMessage msg a@(FireExtinguisher1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifier sid source iid (SkillModifier #combat 1)
      chooseFightEnemy sid iid source
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      sid <- getRandom
      skillTestModifier sid source iid (SkillModifier #agility 3)
      createCardEffect Cards.fireExtinguisher1 Nothing source (SkillTestTarget sid)
      chooseEvadeEnemy sid iid source
      pure a
    _ -> FireExtinguisher1 <$> liftRunMessage msg attrs

newtype FireExtinguisher1Effect = FireExtinguisher1Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fireExtinguisher1Effect :: EffectArgs -> FireExtinguisher1Effect
fireExtinguisher1Effect = cardEffect FireExtinguisher1Effect Cards.fireExtinguisher1

instance RunMessage FireExtinguisher1Effect where
  runMessage msg e@(FireExtinguisher1Effect attrs) = runQueueT $ case msg of
    PassedSkillTest iid (Just Action.Evade) _ (Initiator (EnemyTarget _)) _ _ -> do
      withSkillTest \sid -> do
        when (isTarget sid attrs.target) do
          evasions <- selectMap (EnemyEvaded iid) $ enemyEngagedWith iid
          pushAll evasions
          disable attrs
      pure e
    SkillTestEnds sid _ _ | isTarget sid attrs.target -> disableReturn e
    _ -> FireExtinguisher1Effect <$> liftRunMessage msg attrs

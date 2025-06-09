module Arkham.Asset.Assets.BloodstainedDagger (bloodstainedDagger) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Modifier

newtype BloodstainedDagger = BloodstainedDagger AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodstainedDagger :: AssetCard BloodstainedDagger
bloodstainedDagger = asset BloodstainedDagger Cards.bloodstainedDagger

instance HasAbilities BloodstainedDagger where
  getAbilities (BloodstainedDagger a) =
    [ withTooltip "{action}: _Fight_. You get +2 {combat} for this attack."
        $ restricted a 1 ControlsThis fightAction_
    , withTooltip
        "{action}: Exhaust Bloodstained Dagger and take 1 horror: _Fight_. You get +2 {combat} and deal +1 damage for this attack. If this attack defeats an enemy, draw 1 card."
        $ restricted a 2 ControlsThis
        $ fightAction (exhaust a <> HorrorCost (toSource a) YouTarget 1)
    ]

instance RunMessage BloodstainedDagger where
  runMessage msg a@(BloodstainedDagger attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #combat 2)
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 2) iid [SkillModifier #combat 2, DamageDealt 1]
      chooseFightEnemy sid iid (attrs.ability 2)
      pure a
    EnemyDefeated _ _ (isAbilitySource attrs 2 -> True) _ -> do
      for_ attrs.controller \iid -> drawCards iid (attrs.ability 2) 1
      pure a
    _ -> BloodstainedDagger <$> liftRunMessage msg attrs

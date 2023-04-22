module Arkham.Asset.Cards.Sharpshooter3
  ( sharpshooter3
  , Sharpshooter3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Enemy.Types qualified as Field
import Arkham.Field
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Trait ( Trait (Firearm) )

newtype Sharpshooter3 = Sharpshooter3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sharpshooter3 :: AssetCard Sharpshooter3
sharpshooter3 = asset Sharpshooter3 Cards.sharpshooter3

instance HasAbilities Sharpshooter3 where
  getAbilities (Sharpshooter3 a) =
    [ restrictedAbility a 1 ControlsThis $ ReactionAbility
        (ActivateAbility Timing.When You
        $ AssetAbility (AssetWithTrait Firearm)
        <> AbilityIsAction Action.Fight
        )
        (ExhaustCost (toTarget a))
    ]

instance RunMessage Sharpshooter3 where
  runMessage msg a@(Sharpshooter3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      anyFightableWithEvade <- selectAny $ CanFightEnemy (toAbilitySource attrs 1) <> EnemyWithEvade
      push
        $ chooseOrRunOne
            iid
            $ Label
                "This attack uses {agility} instead of {combat}. All modifiers to your {combat} for this attack modify your instead."
                [ skillTestModifiers
                    attrs
                    iid
                    [ UseSkillInsteadOf SkillCombat SkillAgility
                    , SkillModifiersAffectOtherSkill SkillCombat SkillAgility
                    ]
                ]
        : (if anyFightableWithEvade
             then
               [ Label
                 "Use the attacked enemy's evade value for this attack, instead of their fight value."
                 [ skillTestModifier
                     attrs
                     iid
                     (AlternateFightField (SomeField Field.EnemyEvade))
                 ]
               , Label
                 "Do both"
                 [ skillTestModifiers
                     attrs
                     iid
                     [ UseSkillInsteadOf SkillCombat SkillAgility
                     , SkillModifiersAffectOtherSkill SkillCombat SkillAgility
                     , AlternateFightField (SomeField Field.EnemyEvade)
                     ]
                 ]
               ]
             else [])
      pure a
    _ -> Sharpshooter3 <$> runMessage msg attrs

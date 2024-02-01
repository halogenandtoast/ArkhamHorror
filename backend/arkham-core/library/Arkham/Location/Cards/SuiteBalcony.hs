module Arkham.Location.Cards.SuiteBalcony (
  suiteBalcony,
  SuiteBalcony (..),
)
where

import Arkham.Prelude

import Arkham.DamageEffect
import Arkham.GameValue
import Arkham.Helpers.Enemy
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Trait (Trait (Humanoid))

newtype SuiteBalcony = SuiteBalcony LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

suiteBalcony :: LocationCard SuiteBalcony
suiteBalcony = location SuiteBalcony Cards.suiteBalcony 2 (PerPlayer 1)

instance HasAbilities SuiteBalcony where
  getAbilities (SuiteBalcony attrs) =
    withRevealedAbilities
      attrs
      [ doesNotProvokeAttacksOfOpportunity
          $ restrictedAbility
            attrs
            1
            (Here <> exists (enemyAt (toId attrs) <> EnemyWithTrait Humanoid))
            actionAbility
      ]

instance RunMessage SuiteBalcony where
  runMessage msg l@(SuiteBalcony attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- selectList $ enemyAt (toId attrs) <> EnemyWithTrait Humanoid
      player <- getPlayer iid
      let chooseSkill skill enemy = SkillLabel skill [beginSkillTest iid (toAbilitySource attrs 1) (toTarget enemy) skill 4]
      push
        $ chooseOrRunOne
          player
          [ targetLabel enemy [chooseOne player [chooseSkill #combat enemy, chooseSkill #agility enemy]]
          | enemy <- enemies
          ]
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      mtarget <- getSkillTestTarget
      case mtarget of
        Just (EnemyTarget eid) -> do
          isElite <- eid <=~> EliteEnemy
          defeat <- defeatEnemy eid iid (toAbilitySource attrs 1)
          pushAll $ directHorror iid (toAbilitySource attrs 1) 1
            : if isElite then [Msg.EnemyDamage eid $ nonAttack (toAbilitySource attrs 1) 2] else defeat
        _ -> error "wrong target"
      pure l
    _ -> SuiteBalcony <$> runMessage msg attrs

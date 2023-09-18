module Arkham.Asset.Cards.SurvivalKnife2 (
  survivalKnife2,
  SurvivalKnife2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (EnemyAttacks)
import Arkham.Attack
import Arkham.Id
import Arkham.Matcher
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype SurvivalKnife2 = SurvivalKnife2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

survivalKnife2 :: AssetCard SurvivalKnife2
survivalKnife2 = asset SurvivalKnife2 Cards.survivalKnife2

instance HasAbilities SurvivalKnife2 where
  getAbilities (SurvivalKnife2 a) =
    [ fightAbility a 1 (ActionCost 1) ControlsThis
    , restrictedAbility a 2 (ControlsThis <> DuringPhase #enemy)
        $ ReactionAbility (EnemyAttacks #when You AnyEnemyAttack AnyEnemy) (exhaust a)
    ]

toEnemy :: [Window] -> EnemyId
toEnemy [] = error "called during incorrect window"
toEnemy ((windowType -> Window.EnemyAttacks details) : _) = attackEnemy details
toEnemy (_ : xs) = toEnemy xs

instance RunMessage SurvivalKnife2 where
  runMessage msg a@(SurvivalKnife2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      pushAll
        [ skillTestModifiers source iid [SkillModifier #combat 2]
        , chooseFightEnemy iid source #combat
        ]
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 (toEnemy -> enemy) _ -> do
      let source = toAbilitySource attrs 2
      pushAll
        [ skillTestModifiers source iid [SkillModifier #combat 2, DamageDealt 1]
        , FightEnemy iid enemy source Nothing #combat False
        ]
      pure a
    _ -> SurvivalKnife2 <$> runMessage msg attrs

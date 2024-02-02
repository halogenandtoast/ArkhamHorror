module Arkham.Enemy.Cards.AlejandroVela (
  alejandroVela,
  alejandroVelaEffect,
  AlejandroVela (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Attack
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Story.Cards qualified as Story

newtype AlejandroVela = AlejandroVela EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

alejandroVela :: EnemyCard AlejandroVela
alejandroVela = enemy AlejandroVela Cards.alejandroVela (6, PerPlayer 4, 3) (1, 2)

instance HasAbilities AlejandroVela where
  getAbilities (AlejandroVela a) =
    withBaseAbilities a [restrictedAbility a 1 OnSameLocation parleyAction_]

instance RunMessage AlejandroVela where
  runMessage msg e@(AlejandroVela attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll
        [ createCardEffect Cards.alejandroVela Nothing (toAbilitySource attrs 1) SkillTestTarget
        , parley iid (toAbilitySource attrs 1) attrs #intellect 5
        ]
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ InitiateEnemyAttack $ enemyAttack (toId attrs) (toAbilitySource attrs 1) iid
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ Flip iid (toAbilitySource attrs 1) (toTarget attrs)
      pure e
    Flip iid _ (isTarget attrs -> True) -> do
      anotherWay <- genCard Story.anotherWay
      push $ ReadStory iid anotherWay ResolveIt (Just $ toTarget attrs)
      pure e
    _ -> AlejandroVela <$> runMessage msg attrs

newtype AlejandroVelaEffect = AlejandroVelaEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

alejandroVelaEffect :: EffectArgs -> AlejandroVelaEffect
alejandroVelaEffect = cardEffect AlejandroVelaEffect Cards.alejandroVela

instance HasModifiersFor AlejandroVelaEffect where
  getModifiersFor (ChaosTokenTarget token) (AlejandroVelaEffect a)
    | a.target == SkillTestTarget && token.face == Tablet =
        pure $ toModifiers a [ChangeChaosTokenModifier AutoSuccessModifier]
  getModifiersFor _ _ = pure []

instance RunMessage AlejandroVelaEffect where
  runMessage msg e@(AlejandroVelaEffect attrs) =
    case msg of
      SkillTestEnds _ _ | attrs.target == SkillTestTarget -> do
        push $ disable attrs
        pure e
      _ -> AlejandroVelaEffect <$> runMessage msg attrs

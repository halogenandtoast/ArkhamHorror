module Arkham.Enemy.Cards.IchtacaScionOfYig (
  ichtacaScionOfYig,
  ichtacaScionOfYigEffect,
  IchtacaScionOfYig (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Attack
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.SkillType
import Arkham.Story.Cards qualified as Story

newtype IchtacaScionOfYig = IchtacaScionOfYig EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

ichtacaScionOfYig :: EnemyCard IchtacaScionOfYig
ichtacaScionOfYig =
  enemy IchtacaScionOfYig Cards.ichtacaScionOfYig (4, PerPlayer 6, 4) (2, 1)

instance HasAbilities IchtacaScionOfYig where
  getAbilities (IchtacaScionOfYig a) =
    withBaseAbilities
      a
      [ restrictedAbility a 1 OnSameLocation
          $ ActionAbility [Action.Parley]
          $ ActionCost 1
      ]

instance RunMessage IchtacaScionOfYig where
  runMessage msg e@(IchtacaScionOfYig attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ createCardEffect
            Cards.ichtacaScionOfYig
            Nothing
            (toSource attrs)
            SkillTestTarget
        , parley
            iid
            (toSource attrs)
            (toTarget attrs)
            SkillIntellect
            5
        ]
      pure e
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      push $ InitiateEnemyAttack $ enemyAttack (toId attrs) attrs iid
      pure e
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      push $ Flip iid (toSource attrs) (toTarget attrs)
      pure e
    Flip iid _ target | isTarget attrs target -> do
      yigsMercy <- genCard Story.yigsMercy
      push $ ReadStory iid yigsMercy ResolveIt (Just $ toTarget attrs)
      pure e
    _ -> IchtacaScionOfYig <$> runMessage msg attrs

newtype IchtacaScionOfYigEffect = IchtacaScionOfYigEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

ichtacaScionOfYigEffect :: EffectArgs -> IchtacaScionOfYigEffect
ichtacaScionOfYigEffect =
  cardEffect IchtacaScionOfYigEffect Cards.ichtacaScionOfYig

instance HasModifiersFor IchtacaScionOfYigEffect where
  getModifiersFor (ChaosTokenTarget token) (IchtacaScionOfYigEffect a)
    | effectTarget a == SkillTestTarget && chaosTokenFace token == Cultist =
        pure $ toModifiers a [ChangeChaosTokenModifier AutoSuccessModifier]
  getModifiersFor _ _ = pure []

instance RunMessage IchtacaScionOfYigEffect where
  runMessage msg e@(IchtacaScionOfYigEffect attrs@EffectAttrs {..}) =
    case msg of
      SkillTestEnds _ _ | effectTarget == SkillTestTarget -> do
        push $ DisableEffect effectId
        pure e
      _ -> IchtacaScionOfYigEffect <$> runMessage msg attrs

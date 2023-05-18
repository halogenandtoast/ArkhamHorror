module Arkham.Enemy.Cards.AlejandroVela (
  alejandroVela,
  alejandroVelaEffect,
  AlejandroVela (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Attack
import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Message
import Arkham.SkillType
import Arkham.Story.Cards qualified as Story
import Arkham.Token

newtype AlejandroVela = AlejandroVela EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alejandroVela :: EnemyCard AlejandroVela
alejandroVela =
  enemy AlejandroVela Cards.alejandroVela (6, PerPlayer 4, 3) (1, 2)

instance HasAbilities AlejandroVela where
  getAbilities (AlejandroVela a) =
    withBaseAbilities
      a
      [ restrictedAbility a 1 OnSameLocation $
          ActionAbility (Just Action.Parley) $
            ActionCost 1
      ]

instance RunMessage AlejandroVela where
  runMessage msg e@(AlejandroVela attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ createCardEffect
            Cards.alejandroVela
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
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ ->
      do
        push $ InitiateEnemyAttack $ enemyAttack (toId attrs) iid
        pure e
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ ->
      do
        push $ Flip iid (toSource attrs) (toTarget attrs)
        pure e
    Flip iid _ target | isTarget attrs target -> do
      anotherWay <- genCard Story.anotherWay
      push $ ReadStory iid anotherWay ResolveIt (Just $ toTarget attrs)
      pure e
    _ -> AlejandroVela <$> runMessage msg attrs

newtype AlejandroVelaEffect = AlejandroVelaEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alejandroVelaEffect :: EffectArgs -> AlejandroVelaEffect
alejandroVelaEffect =
  cardEffect AlejandroVelaEffect Cards.alejandroVela

instance HasModifiersFor AlejandroVelaEffect where
  getModifiersFor (TokenTarget token) (AlejandroVelaEffect a)
    | effectTarget a == SkillTestTarget && tokenFace token == Tablet =
        pure $ toModifiers a [ChangeTokenModifier AutoSuccessModifier]
  getModifiersFor _ _ = pure []

instance RunMessage AlejandroVelaEffect where
  runMessage msg e@(AlejandroVelaEffect attrs@EffectAttrs {..}) =
    case msg of
      SkillTestEnds _ _ | effectTarget == SkillTestTarget -> do
        push (DisableEffect effectId)
        pure e
      _ -> AlejandroVelaEffect <$> runMessage msg attrs

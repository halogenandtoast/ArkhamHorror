module Arkham.Enemy.Cards.IchtacaScionOfYig (
  ichtacaScionOfYig,
  ichtacaScionOfYigEffect,
  IchtacaScionOfYig (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Attack
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Story.Cards qualified as Story

newtype IchtacaScionOfYig = IchtacaScionOfYig EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ichtacaScionOfYig :: EnemyCard IchtacaScionOfYig
ichtacaScionOfYig =
  enemy IchtacaScionOfYig Cards.ichtacaScionOfYig (4, PerPlayer 6, 4) (2, 1)

instance HasAbilities IchtacaScionOfYig where
  getAbilities (IchtacaScionOfYig a) =
    withBaseAbilities a [skillTestAbility $ restrictedAbility a 1 OnSameLocation parleyAction_]

instance RunMessage IchtacaScionOfYig where
  runMessage msg e@(IchtacaScionOfYig attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      sid <- getRandom
      enabled <- createCardEffect Cards.ichtacaScionOfYig Nothing attrs sid
      pushAll
        [ enabled
        , parley sid iid attrs attrs #intellect (Fixed 5)
        ]
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ InitiateEnemyAttack $ enemyAttack (toId attrs) attrs iid
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ Flip iid (toSource attrs) (toTarget attrs)
      pure e
    Flip iid _ target | isTarget attrs target -> do
      yigsMercy <- genCard Story.yigsMercy
      push $ ReadStory iid yigsMercy ResolveIt (Just $ toTarget attrs)
      pure e
    _ -> IchtacaScionOfYig <$> runMessage msg attrs

newtype IchtacaScionOfYigEffect = IchtacaScionOfYigEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ichtacaScionOfYigEffect :: EffectArgs -> IchtacaScionOfYigEffect
ichtacaScionOfYigEffect =
  cardEffect IchtacaScionOfYigEffect Cards.ichtacaScionOfYig

instance HasModifiersFor IchtacaScionOfYigEffect where
  getModifiersFor (IchtacaScionOfYigEffect a) =
    getSkillTest >>= \case
      Just st | isTarget st.id a.target -> do
        let tokens = filter ((== Cultist) . (.face)) st.revealedChaosTokens
        modifyEach a (map ChaosTokenTarget tokens) [ChangeChaosTokenModifier AutoSuccessModifier]
      _ -> pure mempty

instance RunMessage IchtacaScionOfYigEffect where
  runMessage msg e@(IchtacaScionOfYigEffect attrs@EffectAttrs {..}) =
    case msg of
      SkillTestEnds sid _ _ | isTarget sid effectTarget -> do
        push $ DisableEffect effectId
        pure e
      _ -> IchtacaScionOfYigEffect <$> runMessage msg attrs

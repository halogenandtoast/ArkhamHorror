module Arkham.Enemy.Cards.IchtacaScionOfYig (ichtacaScionOfYig, ichtacaScionOfYigEffect) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Helpers.Story
import Arkham.Story.Cards qualified as Story

newtype IchtacaScionOfYig = IchtacaScionOfYig EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ichtacaScionOfYig :: EnemyCard IchtacaScionOfYig
ichtacaScionOfYig = enemy IchtacaScionOfYig Cards.ichtacaScionOfYig (4, PerPlayer 6, 4) (2, 1)

instance HasAbilities IchtacaScionOfYig where
  getAbilities (IchtacaScionOfYig a) =
    extend1 a $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage IchtacaScionOfYig where
  runMessage msg e@(IchtacaScionOfYig attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      createCardEffect Cards.ichtacaScionOfYig Nothing attrs sid
      parley sid iid attrs attrs #intellect (Fixed 5)
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      initiateEnemyAttack attrs attrs iid
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      readStory iid attrs Story.yigsMercy
      pure e
    _ -> IchtacaScionOfYig <$> liftRunMessage msg attrs

newtype IchtacaScionOfYigEffect = IchtacaScionOfYigEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ichtacaScionOfYigEffect :: EffectArgs -> IchtacaScionOfYigEffect
ichtacaScionOfYigEffect = cardEffect IchtacaScionOfYigEffect Cards.ichtacaScionOfYig

instance HasModifiersFor IchtacaScionOfYigEffect where
  getModifiersFor (IchtacaScionOfYigEffect a) =
    whenJustM getSkillTest \st -> do
      when (isTarget st.id a.target) do
        let tokens = filter ((== Cultist) . (.face)) st.revealedChaosTokens
        modifyEach a (map ChaosTokenTarget tokens) [ChangeChaosTokenModifier AutoSuccessModifier]

instance RunMessage IchtacaScionOfYigEffect where
  runMessage msg e@(IchtacaScionOfYigEffect attrs) = runQueueT $ case msg of
    SkillTestEnds sid _ _ | isTarget sid attrs.target -> do
      disableReturn e
    _ -> IchtacaScionOfYigEffect <$> liftRunMessage msg attrs

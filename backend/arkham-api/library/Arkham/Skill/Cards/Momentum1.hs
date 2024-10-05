module Arkham.Skill.Cards.Momentum1 (momentum1, momentum1Effect, Momentum1 (..)) where

import Arkham.Effect.Import
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestInvestigator)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Momentum1 = Momentum1 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

momentum1 :: SkillCard Momentum1
momentum1 = skill Momentum1 Cards.momentum1

instance RunMessage Momentum1 where
  runMessage msg s@(Momentum1 attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ n | n > 0 -> do
      afterSkillTest $ createCardEffect Cards.momentum1 (Just $ EffectInt (min 3 n)) attrs attrs.owner
      pure s
    _ -> Momentum1 <$> liftRunMessage msg attrs

newtype Momentum1Effect = Momentum1Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

momentum1Effect :: EffectArgs -> Momentum1Effect
momentum1Effect = cardEffect Momentum1Effect Cards.momentum1

instance HasModifiersFor Momentum1Effect where
  getModifiersFor (SkillTestTarget _) (Momentum1Effect attrs) = maybeModified attrs do
    investigator <- MaybeT getSkillTestInvestigator
    guard $ investigator `is` attrs.target
    EffectInt n <- hoistMaybe attrs.meta
    pure [Difficulty (-n)]
  getModifiersFor _ _ = pure []

instance RunMessage Momentum1Effect where
  runMessage msg e@(Momentum1Effect attrs) = runQueueT $ case msg of
    EndPhase -> disableReturn e
    SkillTestEnds _ iid _ | iid `is` attrs.target -> disableReturn e
    _ -> Momentum1Effect <$> liftRunMessage msg attrs

module Arkham.Skill.Cards.Momentum1 (
  momentum1,
  momentum1Effect,
  Momentum1 (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Helpers.Modifiers
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Momentum1 = Momentum1 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

momentum1 :: SkillCard Momentum1
momentum1 = skill Momentum1 Cards.momentum1

instance RunMessage Momentum1 where
  runMessage msg s@(Momentum1 attrs) = case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ n | n > 0 -> do
      push $ createCardEffect Cards.momentum1 (Just $ EffectInt (min 3 n)) attrs (skillOwner attrs)
      pure s
    _ -> Momentum1 <$> runMessage msg attrs

newtype Momentum1Effect = Momentum1Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

momentum1Effect :: EffectArgs -> Momentum1Effect
momentum1Effect = cardEffect Momentum1Effect Cards.momentum1

instance HasModifiersFor Momentum1Effect where
  getModifiersFor SkillTestTarget (Momentum1Effect attrs) = do
    mMods <- runMaybeT $ do
      investigator <- MaybeT getSkillTestInvestigator
      guard $ investigator `is` effectTarget attrs
      EffectInt n <- hoistMaybe $ effectMetadata attrs
      pure $ Difficulty (-n)
    pure $ toModifiers attrs $ maybeToList mMods
  getModifiersFor _ _ = pure []

instance RunMessage Momentum1Effect where
  runMessage msg e@(Momentum1Effect attrs) = case msg of
    EndPhase -> do
      push $ disable attrs
      pure e
    SkillTestEnds iid _ | iid `is` attrs.target -> do
      push $ disable attrs
      pure e
    _ -> Momentum1Effect <$> runMessage msg attrs

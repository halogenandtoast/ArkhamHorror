module Arkham.Skill.Cards.QuickThinking (quickThinking, quickThinkingEffect, QuickThinking (..)) where

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype QuickThinking = QuickThinking SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickThinking :: SkillCard QuickThinking
quickThinking = skill QuickThinking Cards.quickThinking

instance RunMessage QuickThinking where
  runMessage msg s@(QuickThinking attrs) = case msg of
    PassedSkillTest iid _ _ SkillTestInitiatorTarget {} _ n | n >= 2 -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label "Take additional action" [createCardEffect Cards.quickThinking Nothing attrs iid]
          , Label "Pass on additional action" []
          ]
      pure s
    _ -> QuickThinking <$> runMessage msg attrs

newtype QuickThinkingEffect = QuickThinkingEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickThinkingEffect :: EffectArgs -> QuickThinkingEffect
quickThinkingEffect = cardEffect QuickThinkingEffect Cards.quickThinking

instance RunMessage QuickThinkingEffect where
  runMessage msg e@(QuickThinkingEffect attrs) = case msg of
    AfterSkillTestEnds {} -> case effectTarget attrs of
      InvestigatorTarget iid -> do
        pushAll [disable attrs, GainActions iid attrs.source 1, PlayerWindow iid [] False]
        pure e
      _ -> error "wrong target"
    _ -> QuickThinkingEffect <$> runMessage msg attrs

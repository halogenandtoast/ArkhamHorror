module Arkham.Skill.Cards.QuickThinking (quickThinking, QuickThinking (..)) where

import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), withoutModifier)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.Taboo

newtype QuickThinking = QuickThinking SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickThinking :: SkillCard QuickThinking
quickThinking = skill QuickThinking Cards.quickThinking

instance RunMessage QuickThinking where
  runMessage msg s@(QuickThinking attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ n | n >= 2 -> do
      canRun <- withoutModifier (CardCodeTarget $ toCardCode attrs) Semaphore
      when canRun do
        when (tabooed TabooList18 attrs) do
          roundModifier attrs (CardCodeTarget $ toCardCode attrs) Semaphore
        chooseOne
          iid
          [ Label "Take additional action" [DoStep 1 msg]
          , Label "Pass on additional action" []
          ]
      pure s
    DoStep 1 (PassedSkillTest iid _ _ (isTarget attrs -> True) _ _) -> do
      afterSkillTest do
        pushAll [GainActions iid (toSource attrs) 1, PlayerWindow iid [] False]
      pure s
    _ -> QuickThinking <$> liftRunMessage msg attrs

module Arkham.Skill.Cards.QuickThinking (quickThinking, QuickThinking (..)) where

import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), withoutModifier)
import Arkham.Message.Lifted.Choose
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
      whenM (withoutModifier (CardCodeTarget $ toCardCode attrs) Semaphore) do
        when (tabooed TabooList18 attrs) $ roundModifier attrs (CardCodeTarget $ toCardCode attrs) Semaphore
        chooseOneM iid do
          labeled "Take additional action" $ doStep 1 msg
          labeled "Pass on additional action" nothing
      pure s
    DoStep 1 (PassedSkillTest _ _ _ (isTarget attrs -> True) _ _) -> do
      afterSkillTest do
        pushAll [GainActions attrs.controller (toSource attrs) 1, PlayerWindow attrs.controller [] False]
      pure s
    _ -> QuickThinking <$> liftRunMessage msg attrs

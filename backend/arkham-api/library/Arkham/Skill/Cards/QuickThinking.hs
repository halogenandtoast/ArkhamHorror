module Arkham.Skill.Cards.QuickThinking (quickThinking) where

import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), semaphore)
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
      semaphore (toCardCode attrs) do
        when (tabooed TabooList18 attrs) $ roundModifier attrs (toCardCode attrs) Semaphore
        chooseOneM iid do
          labeled "Take additional action" $ doStep 1 msg
          labeled "Pass on additional action" nothing
      pure s
    DoStep 1 (PassedSkillTest iid _ _ (isTarget attrs -> True) _ _) -> do
      afterSkillTest iid "Quick Thinking" $ takeActionAsIfTurn attrs.controller attrs
      pure s
    _ -> QuickThinking <$> liftRunMessage msg attrs

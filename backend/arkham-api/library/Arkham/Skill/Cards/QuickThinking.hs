module Arkham.Skill.Cards.QuickThinking (quickThinking) where

import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Helpers.Modifiers (ModifierType (..), semaphore)
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult
import Arkham.Taboo

newtype QuickThinking = QuickThinking SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickThinking :: SkillCard QuickThinking
quickThinking = skill QuickThinking Cards.quickThinking

instance RunMessage QuickThinking where
  runMessage msg s@(QuickThinking attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy _ n | n >= 2 -> do
              semaphore (toCardCode attrs) do
                when (tabooed TabooList18 attrs) $ roundModifier attrs (toCardCode attrs) Semaphore
                provideSkillTestResultOption attrs exclusions "Quick Thinking" do
                  let passedMsg = PassedSkillTest st.investigator st.action st.source st.target st.kind n
                  chooseOneM st.investigator do
                    labeled "Take additional action" $ doStep 1 passedMsg
                    labeled "Pass on additional action" nothing
            _ -> pure ()
      pure s
    DoStep 1 (PassedSkillTest iid _ _ (isTarget attrs -> True) _ _) -> do
      afterSkillTest iid "Quick Thinking" $ takeActionAsIfTurn attrs.controller attrs
      pure s
    _ -> QuickThinking <$> liftRunMessage msg attrs

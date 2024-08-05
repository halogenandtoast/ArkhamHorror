module Arkham.Skill.Cards.QuickThinking (quickThinking, QuickThinking (..)) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype QuickThinking = QuickThinking SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickThinking :: SkillCard QuickThinking
quickThinking = skill QuickThinking Cards.quickThinking

instance RunMessage QuickThinking where
  runMessage msg s@(QuickThinking attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ n | n >= 2 -> do
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

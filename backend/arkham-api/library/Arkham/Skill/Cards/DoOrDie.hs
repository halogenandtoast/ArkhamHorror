module Arkham.Skill.Cards.DoOrDie (doOrDie) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype DoOrDie = DoOrDie SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

doOrDie :: SkillCard DoOrDie
doOrDie = skill DoOrDie Cards.doOrDie

instance RunMessage DoOrDie where
  runMessage msg s@(DoOrDie attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      cards <- select $ InDiscardOf (InvestigatorWithId iid) <> oneOf [#asset, #event] <> #survivor
      focusCards cards $ chooseTargetM iid cards $ addToHand iid . only
      pure s
    _ -> DoOrDie <$> liftRunMessage msg attrs

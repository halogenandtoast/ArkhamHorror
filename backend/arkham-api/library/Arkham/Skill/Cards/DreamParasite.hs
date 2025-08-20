module Arkham.Skill.Cards.DreamParasite (dreamParasite) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype DreamParasite = DreamParasite SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamParasite :: SkillCard DreamParasite
dreamParasite = skill DreamParasite Cards.dreamParasite

instance RunMessage DreamParasite where
  runMessage msg s@(DreamParasite attrs) = runQueueT $ case msg of
    FailedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      assignDamageAndHorror iid attrs 1 1
      pure s
    _ -> DreamParasite <$> liftRunMessage msg attrs

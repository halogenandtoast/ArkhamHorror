module Arkham.Skill.Cards.Guts (guts) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Guts = Guts SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guts :: SkillCard Guts
guts = skill Guts Cards.guts

instance RunMessage Guts where
  runMessage msg s@(Guts attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      let drawer = if attrs.cardCode.isChapterTwo then iid else attrs.owner
      additionalSkillTestOption "Guts" $ drawCards drawer attrs 1
      pure s
    _ -> Guts <$> liftRunMessage msg attrs

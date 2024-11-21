module Arkham.Skill.Cards.TakeHeart (takeHeart, TakeHeart (..)) where

import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype TakeHeart = TakeHeart SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

takeHeart :: SkillCard TakeHeart
takeHeart = skill TakeHeart Cards.takeHeart

instance RunMessage TakeHeart where
  runMessage msg s@(TakeHeart attrs) = runQueueT $ case msg of
    FailedSkillTest iid _ _ (SkillTarget sid) _ _ | sid == toId attrs -> do
      drawCardsIfCan iid attrs 2
      gainResourcesIfCan iid attrs 2
      pure s
    _ -> TakeHeart <$> liftRunMessage msg attrs

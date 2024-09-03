module Arkham.Skill.Cards.AsYouWish (asYouWish, AsYouWish (..)) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype AsYouWish = AsYouWish SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

asYouWish :: SkillCard AsYouWish
asYouWish = skill AsYouWish Cards.asYouWish

instance RunMessage AsYouWish where
  runMessage msg s@(AsYouWish attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      drawCardsIfCan iid attrs 1
      pure s
    FailedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      drawCardsIfCan attrs.owner attrs 1
      pure s
    _ -> AsYouWish <$> liftRunMessage msg attrs

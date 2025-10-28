module Arkham.Skill.Cards.AllIn5 (allIn5) where

import Arkham.Draw.Types
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.Taboo

newtype AllIn5 = AllIn5 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allIn5 :: SkillCard AllIn5
allIn5 = skill AllIn5 Cards.allIn5

instance RunMessage AllIn5 where
  runMessage msg s@(AllIn5 attrs) = runQueueT $ case msg of
    InvestigatorCommittedSkill _ (is attrs -> True) -> do
      attrs' <- liftRunMessage msg attrs
      pure
        $ AllIn5
        $ attrs'
        & if tabooed TabooList18 attrs' then afterPlayL .~ RemoveThisFromGame else id
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      skillTestResultOption "All In" do
        drawCardsEdit iid attrs 5 shuffleBackInEachWeakness
      pure s
    _ -> AllIn5 <$> liftRunMessage msg attrs

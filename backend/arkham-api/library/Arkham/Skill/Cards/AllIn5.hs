module Arkham.Skill.Cards.AllIn5 (allIn5) where

import Arkham.Draw.Types
import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult
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
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy _ n -> do
              provideSkillTestResultOption attrs exclusions "All In" do
                drawCardsEdit st.investigator attrs (min 5 n) shuffleBackInEachWeakness
            _ -> pure ()
      pure s
    _ -> AllIn5 <$> liftRunMessage msg attrs

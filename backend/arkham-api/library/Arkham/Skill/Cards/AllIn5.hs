module Arkham.Skill.Cards.AllIn5 (allIn5, AllIn5 (..)) where

import Arkham.Classes
import Arkham.Draw.Types
import Arkham.Message
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner
import Arkham.Taboo

newtype AllIn5 = AllIn5 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allIn5 :: SkillCard AllIn5
allIn5 = skill AllIn5 Cards.allIn5

instance RunMessage AllIn5 where
  runMessage msg s@(AllIn5 attrs) = case msg of
    InvestigatorCommittedSkill _ skillId | skillId == attrs.id -> do
      attrs' <- runMessage msg attrs
      pure
        $ AllIn5
        $ attrs'
        & if tabooed TabooList18 attrs' then afterPlayL .~ RemoveThisFromGame else id
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      push $ DrawCards iid $ withCardDrawRule ShuffleBackInEachWeakness $ newCardDraw attrs iid 5
      pure s
    _ -> AllIn5 <$> runMessage msg attrs

module Arkham.Skill.Cards.DoubleDown2 (doubleDown2) where

import Arkham.Helpers.Cost (getSpendableResources)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Modifier
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype DoubleDown2 = DoubleDown2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

doubleDown2 :: SkillCard DoubleDown2
doubleDown2 = skill DoubleDown2 Cards.doubleDown2

instance RunMessage DoubleDown2 where
  runMessage msg s@(DoubleDown2 attrs) = runQueueT $ case msg of
    InvestigatorCommittedSkill iid sid | sid == attrs.id -> do
      n <- getSpendableResources iid
      when (n > 0) do
        chooseAmount iid "Spend resources on Double Down" "Resources" 0 (min 3 n) attrs
      pure s
    ResolveAmounts iid (getChoiceAmount "Resources" -> n) (isTarget attrs -> True) -> do
      when (n > 0) do
        withSkillTest \sid -> do
          spendResources iid n
          skillTestModifier sid attrs attrs.cardId $ AddSkillIcons (mconcat $ replicate n [#wild, #wild])
      pure s
    _ -> DoubleDown2 <$> liftRunMessage msg attrs

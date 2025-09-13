module Arkham.Skill.Cards.CalculatedRisk (calculatedRisk) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype CalculatedRisk = CalculatedRisk SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

calculatedRisk :: SkillCard CalculatedRisk
calculatedRisk = skill CalculatedRisk Cards.calculatedRisk

instance HasModifiersFor CalculatedRisk where
  getModifiersFor (CalculatedRisk attrs) = do
    n <- fieldMap InvestigatorActionsTaken length attrs.owner
    addSkillIcons attrs $ replicate (n + 1) #wild

instance RunMessage CalculatedRisk where
  runMessage msg (CalculatedRisk attrs) = runQueueT $ case msg of
    InvestigatorCommittedSkill iid sid | sid == toId attrs -> do
      afterSkillTest iid "Calculated Risk" $ push $ ChooseEndTurn iid
      CalculatedRisk <$> liftRunMessage msg attrs
    _ -> CalculatedRisk <$> liftRunMessage msg attrs

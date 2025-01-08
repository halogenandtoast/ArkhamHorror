module Arkham.Skill.Cards.CalculatedRisk (calculatedRisk, CalculatedRisk (..)) where

import Arkham.Helpers.Modifiers
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
    modifySelf attrs.cardId [AddSkillIcons $ replicate n #wild]

instance RunMessage CalculatedRisk where
  runMessage msg (CalculatedRisk attrs) = runQueueT $ case msg of
    InvestigatorCommittedSkill iid sid | sid == toId attrs -> do
      afterSkillTest $ push $ ChooseEndTurn iid
      CalculatedRisk <$> liftRunMessage msg attrs
    _ -> CalculatedRisk <$> liftRunMessage msg attrs

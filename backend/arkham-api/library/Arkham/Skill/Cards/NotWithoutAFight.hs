module Arkham.Skill.Cards.NotWithoutAFight (notWithoutAFight) where

import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype NotWithoutAFight = NotWithoutAFight SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

notWithoutAFight :: SkillCard NotWithoutAFight
notWithoutAFight = skill NotWithoutAFight Cards.notWithoutAFight

instance HasModifiersFor NotWithoutAFight where
  getModifiersFor (NotWithoutAFight attrs) = do
    n <- selectCount $ EnemyIsEngagedWith $ InvestigatorWithId attrs.owner
    addSkillIcons attrs $ cycleN n [#willpower, #combat, #agility]

instance RunMessage NotWithoutAFight where
  runMessage msg (NotWithoutAFight attrs) = NotWithoutAFight <$> runMessage msg attrs

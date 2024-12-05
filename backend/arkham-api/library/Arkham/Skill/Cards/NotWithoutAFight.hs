module Arkham.Skill.Cards.NotWithoutAFight (notWithoutAFight, NotWithoutAFight (..)) where

import Arkham.Card
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype NotWithoutAFight = NotWithoutAFight SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

notWithoutAFight :: SkillCard NotWithoutAFight
notWithoutAFight = skill NotWithoutAFight Cards.notWithoutAFight

instance HasModifiersFor NotWithoutAFight where
  getModifiersFor (NotWithoutAFight attrs) = do
    n <- selectCount $ EnemyIsEngagedWith $ InvestigatorWithId attrs.owner
    modified_
      attrs
      (CardIdTarget $ toCardId attrs)
      [AddSkillIcons $ cycleN n [#willpower, #combat, #agility]]

instance RunMessage NotWithoutAFight where
  runMessage msg (NotWithoutAFight attrs) = NotWithoutAFight <$> runMessage msg attrs

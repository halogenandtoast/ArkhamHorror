module Arkham.Skill.Cards.NotWithoutAFight
  ( notWithoutAFight
  , NotWithoutAFight(..)
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner
import Arkham.SkillType
import Arkham.Target

newtype NotWithoutAFight = NotWithoutAFight SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

notWithoutAFight :: SkillCard NotWithoutAFight
notWithoutAFight = skill NotWithoutAFight Cards.notWithoutAFight

instance HasModifiersFor NotWithoutAFight where
  getModifiersFor (CardIdTarget cid) (NotWithoutAFight attrs)
    | toCardId attrs == cid = do
      n <- selectCount $ EnemyIsEngagedWith $ InvestigatorWithId $ skillOwner
        attrs
      pure $ toModifiers
        attrs
        [AddSkillIcons $ cycleN n [SkillWillpower, SkillCombat, SkillAgility]]
  getModifiersFor _ _ = pure []

instance RunMessage NotWithoutAFight where
  runMessage msg (NotWithoutAFight attrs) =
    NotWithoutAFight <$> runMessage msg attrs

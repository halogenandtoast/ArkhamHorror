module Arkham.Skill.Cards.StrengthInNumbers1 (strengthInNumbers1, StrengthInNumbers1 (..)) where

import Arkham.Classes
import Arkham.Helpers.Calculation
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype StrengthInNumbers1 = StrengthInNumbers1 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strengthInNumbers1 :: SkillCard StrengthInNumbers1
strengthInNumbers1 = skill StrengthInNumbers1 Cards.strengthInNumbers1

instance HasModifiersFor StrengthInNumbers1 where
  getModifiersFor (CardIdTarget cid) (StrengthInNumbers1 a) | a.cardId == cid = do
    n <- calculate (DifferentClassAmong $ ControlledBy $ InvestigatorWithId a.owner)
    modified a [AddSkillIcons $ replicate n #wild]
  getModifiersFor target (StrengthInNumbers1 a) | a `is` target = do
    n <- calculate (DifferentClassAmong $ ControlledBy $ InvestigatorWithId a.owner)
    modified a [AddSkillIcons $ replicate n #wild]
  getModifiersFor _ _ = pure []

instance RunMessage StrengthInNumbers1 where
  runMessage msg (StrengthInNumbers1 attrs) = StrengthInNumbers1 <$> runMessage msg attrs

module Arkham.Skill.Cards.DefensiveStance1 (
  defensiveStance1,
  DefensiveStance1 (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype DefensiveStance1 = DefensiveStance1 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

defensiveStance1 :: SkillCard DefensiveStance1
defensiveStance1 = skill DefensiveStance1 Cards.defensiveStance1

instance HasModifiersFor DefensiveStance1 where
  getModifiersFor (CardIdTarget cid) (DefensiveStance1 a) | toCardId a == cid = do
    agility <- field InvestigatorAgility (skillOwner a)
    combat <- field InvestigatorCombat (skillOwner a)
    pure $ toModifiers a [AddSkillIcons $ replicate combat #agility <> replicate agility #combat]
  getModifiersFor target (DefensiveStance1 a) | a `is` target = do
    agility <- field InvestigatorAgility (skillOwner a)
    combat <- field InvestigatorCombat (skillOwner a)
    pure $ toModifiers a [AddSkillIcons $ replicate combat #agility <> replicate agility #combat]
  getModifiersFor _ _ = pure []

instance RunMessage DefensiveStance1 where
  runMessage msg (DefensiveStance1 attrs) = DefensiveStance1 <$> runMessage msg attrs

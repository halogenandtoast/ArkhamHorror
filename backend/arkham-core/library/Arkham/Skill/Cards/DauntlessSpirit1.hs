module Arkham.Skill.Cards.DauntlessSpirit1 (
  dauntlessSpirit1,
  DauntlessSpirit1 (..),
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

newtype DauntlessSpirit1 = DauntlessSpirit1 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

dauntlessSpirit1 :: SkillCard DauntlessSpirit1
dauntlessSpirit1 = skill DauntlessSpirit1 Cards.dauntlessSpirit1

instance HasModifiersFor DauntlessSpirit1 where
  getModifiersFor (CardIdTarget cid) (DauntlessSpirit1 a) | toCardId a == cid = do
    willpower <- field InvestigatorWillpower (skillOwner a)
    combat <- field InvestigatorCombat (skillOwner a)
    pure $ toModifiers a [AddSkillIcons $ replicate combat #willpower <> replicate willpower #combat]
  getModifiersFor target (DauntlessSpirit1 a) | a `is` target = do
    willpower <- field InvestigatorWillpower (skillOwner a)
    combat <- field InvestigatorCombat (skillOwner a)
    pure $ toModifiers a [AddSkillIcons $ replicate combat #willpower <> replicate willpower #combat]
  getModifiersFor _ _ = pure []

instance RunMessage DauntlessSpirit1 where
  runMessage msg (DauntlessSpirit1 attrs) = DauntlessSpirit1 <$> runMessage msg attrs

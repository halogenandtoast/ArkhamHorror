module Arkham.Skill.Cards.DauntlessSpirit1 (dauntlessSpirit1, DauntlessSpirit1 (..)) where

import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype DauntlessSpirit1 = DauntlessSpirit1 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dauntlessSpirit1 :: SkillCard DauntlessSpirit1
dauntlessSpirit1 = skill DauntlessSpirit1 Cards.dauntlessSpirit1

instance HasModifiersFor DauntlessSpirit1 where
  getModifiersFor (DauntlessSpirit1 a) = do
    willpower <- field InvestigatorWillpower (skillOwner a)
    combat <- field InvestigatorCombat (skillOwner a)
    modifySelf a.cardId [AddSkillIcons $ replicate combat #willpower <> replicate willpower #combat]

instance RunMessage DauntlessSpirit1 where
  runMessage msg (DauntlessSpirit1 attrs) = DauntlessSpirit1 <$> runMessage msg attrs

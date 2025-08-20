module Arkham.Skill.Cards.Savant1 (savant1) where

import Arkham.Helpers.Investigator (modifiedStatsOf)
import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestSkillTypes)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillType
import Arkham.Stats

newtype Savant1 = Savant1 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

savant1 :: SkillCard Savant1
savant1 = skill Savant1 Cards.savant1

instance HasModifiersFor Savant1 where
  getModifiersFor (Savant1 attrs) = do
    sTypes <- getSkillTestSkillTypes
    let types = filter (`notElem` sTypes) [#willpower, #intellect, #combat, #agility]
    sAction <- getSkillTestAction
    stats <- modifiedStatsOf sAction attrs.owner
    let
      getType = \case
        SkillWillpower -> stats.willpower
        SkillIntellect -> stats.intellect
        SkillCombat -> stats.combat
        SkillAgility -> stats.agility
    let n = fromMaybe 0 $ minimumMay (map getType types)
    addSkillIcons attrs $ replicate n #wild

instance RunMessage Savant1 where
  runMessage msg (Savant1 attrs) = Savant1 <$> runMessage msg attrs

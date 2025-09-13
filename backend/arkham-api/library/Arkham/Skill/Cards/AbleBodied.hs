module Arkham.Skill.Cards.AbleBodied (ableBodied) where

import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype AbleBodied = AbleBodied SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ableBodied :: SkillCard AbleBodied
ableBodied = skill AbleBodied Cards.ableBodied

instance HasModifiersFor AbleBodied where
  getModifiersFor (AbleBodied a) = do
    itemAssets <- selectCount $ #item <> assetControlledBy a.owner
    addSkillIconsWhen a (itemAssets <= 2)
      $ if itemAssets <= 1
        then [#combat, #combat, #agility, #agility]
        else [#combat, #agility]

instance RunMessage AbleBodied where
  runMessage msg (AbleBodied attrs) = AbleBodied <$> runMessage msg attrs

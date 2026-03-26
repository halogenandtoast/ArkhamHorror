module Arkham.Skill.Cards.EasyStreet (easyStreet) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype EasyStreet = EasyStreet SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easyStreet :: SkillCard EasyStreet
easyStreet = skill EasyStreet Cards.easyStreet

instance HasModifiersFor EasyStreet where
  getModifiersFor (EasyStreet attrs) = do
    resources <- field InvestigatorResources attrs.owner
    let wilds = min 3 (resources `div` 3)
    addSkillIconsWhen attrs (wilds > 0) (replicate wilds #wild)

instance RunMessage EasyStreet where
  runMessage msg (EasyStreet attrs) = EasyStreet <$> runMessage msg attrs

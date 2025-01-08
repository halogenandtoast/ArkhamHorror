module Arkham.Skill.Cards.LastChance (lastChance, LastChance (..)) where

import Arkham.Card
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillType

newtype LastChance = LastChance SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lastChance :: SkillCard LastChance
lastChance = skill LastChance Cards.lastChance

instance HasModifiersFor LastChance where
  getModifiersFor (LastChance a) = do
    hand <- field InvestigatorHand a.owner
    let n = max 0 $ length hand - (if any ((== a.cardId) . toCardId) hand then 1 else 0)
    modifiedWhen_ a (n > 0) a.cardId [RemoveSkillIcons $ replicate n WildIcon]

instance RunMessage LastChance where
  runMessage msg (LastChance attrs) = LastChance <$> runMessage msg attrs

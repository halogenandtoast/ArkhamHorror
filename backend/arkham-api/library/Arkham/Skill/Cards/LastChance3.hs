module Arkham.Skill.Cards.LastChance3 (lastChance3) where

import Arkham.Card
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillType

newtype LastChance3 = LastChance3 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lastChance3 :: SkillCard LastChance3
lastChance3 = skill LastChance3 Cards.lastChance3

instance HasModifiersFor LastChance3 where
  getModifiersFor (LastChance3 a) = do
    hand <- field InvestigatorHand a.owner
    let n = max 0 $ length hand - (if any ((== a.cardId) . toCardId) hand then 1 else 0)
    modifiedWhen_ a (n > 0) a.cardId [RemoveSkillIcons $ replicate n WildIcon]

instance RunMessage LastChance3 where
  runMessage msg s@(LastChance3 attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      drawCards attrs.owner attrs 2
      pure s
    _ -> LastChance3 <$> liftRunMessage msg attrs

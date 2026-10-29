module Arkham.Skill.Cards.Daring (daring) where

import Arkham.Helpers.SkillTest
import Arkham.Message
import Arkham.Placement
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.Zone

newtype Daring = Daring SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daring :: SkillCard Daring
daring = skill Daring Cards.daring

instance RunMessage Daring where
  runMessage msg s@(Daring attrs) = runQueueT $ case msg of
    InvestigatorCommittedSkill _ sid | sid == attrs.id -> do
      withSkillTestTargetedEnemy \enemy -> do
        withSkillTest \stid -> do
          skillTestModifiers stid attrs enemy [#retaliate, #alert]
      Daring <$> liftRunMessage msg attrs
    SkillTestEnds {} | attrs.placement /= OutOfPlay RemovedZone -> do
      drawCards attrs.owner attrs 1
      pure s
    _ -> Daring <$> liftRunMessage msg attrs

module Arkham.Skill.Cards.LongShot (longShot, LongShot (..)) where

import Arkham.Action
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Helpers.SkillTest.Target
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype LongShot = LongShot SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

longShot :: SkillCard LongShot
longShot = skill LongShot Cards.longShot

instance HasModifiersFor LongShot where
  getModifiersFor target (LongShot attrs) = maybeModified attrs do
    guard $ isTarget attrs target
    pure
      [ CanCommitToSkillTestPerformedByAnInvestigatorAt
          (ConnectedFrom $ locationWithInvestigator attrs.owner)
      ]

instance RunMessage LongShot where
  runMessage msg s@(LongShot attrs) = runQueueT $ case msg of
    PassedSkillTest _iid (Just Fight) _ (isTarget attrs -> True) _ _ -> do
      whenJustM getSkillTestTarget \case
        EnemyTarget eid -> nonAttackEnemyDamage attrs 1 eid
        _ -> error "invalid target"
      pure s
    PassedSkillTest _iid (Just Evade) _ (isTarget attrs -> True) _ _ -> do
      whenJustM getSkillTestTarget \case
        EnemyTarget eid -> nonAttackEnemyDamage attrs 1 eid
        _ -> error "invalid target"
      pure s
    _ -> LongShot <$> liftRunMessage msg attrs

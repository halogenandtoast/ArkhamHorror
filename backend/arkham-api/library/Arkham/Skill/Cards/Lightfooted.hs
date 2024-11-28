module Arkham.Skill.Cards.Lightfooted (lightfooted, Lightfooted (..)) where

import Arkham.Helpers.SkillTest (getSkillTestTarget, isEvading)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Lightfooted = Lightfooted SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lightfooted :: SkillCard Lightfooted
lightfooted = skill Lightfooted Cards.lightfooted

instance RunMessage Lightfooted where
  runMessage msg s@(Lightfooted attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      void $ runMaybeT do
        EnemyTarget eid <- MaybeT getSkillTestTarget
        liftGuardM $ isEvading eid
        otherEnemies <-
          select $ enemyAtLocationWith attrs.owner <> EnemyCanBeEvadedBy (toSource attrs) <> not_ (be eid)
        guard $ notNull otherEnemies
        lift $ chooseOneM attrs.owner do
          labeled "Do not evade another enemy" nothing
          targets otherEnemies $ automaticallyEvadeEnemy attrs.owner
      pure s
    _ -> Lightfooted <$> liftRunMessage msg attrs

module Arkham.Skill.Cards.Lightfooted (lightfooted, Lightfooted (..)) where

import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestTarget)
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
      getSkillTestTarget >>= \case
        Just (EnemyTarget eid) -> do
          mAction <- getSkillTestAction
          when (#evade `elem` mAction) do
            otherEnemies <-
              select
                $ enemyAtLocationWith attrs.owner
                <> EnemyCanBeEvadedBy (toSource attrs)
                <> not_ (EnemyWithId eid)
            when (notNull otherEnemies) do
              chooseOneM attrs.owner do
                labeled "Do not evade another enemy" nothing
                for_ otherEnemies \enemy -> do
                  targeting enemy $ automaticallyEvadeEnemy attrs.owner eid
        _ -> pure ()
      pure s
    _ -> Lightfooted <$> liftRunMessage msg attrs

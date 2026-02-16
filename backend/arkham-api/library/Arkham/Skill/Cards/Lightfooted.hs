module Arkham.Skill.Cards.Lightfooted (lightfooted) where

import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy, isEvading)
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
        eid <- MaybeT getSkillTestTargetedEnemy
        liftGuardM $ isEvading eid
        otherEnemies <-
          select $ enemyAtLocationWith attrs.owner <> EnemyCanBeEvadedBy (toSource attrs) <> not_ (be eid)
        concealed <- getConcealedIds (ForExpose $ toSource attrs) attrs.owner
        guard $ notNull otherEnemies
        lift $ additionalSkillTestOption "Lightfooted" do
          chooseOneM attrs.owner do
            labeled "Do not evade another enemy" nothing
            targets otherEnemies $ automaticallyEvadeEnemy attrs.owner
            targets concealed $ exposeConcealed attrs.owner attrs
      pure s
    _ -> Lightfooted <$> liftRunMessage msg attrs

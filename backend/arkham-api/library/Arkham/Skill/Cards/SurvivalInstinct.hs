module Arkham.Skill.Cards.SurvivalInstinct (survivalInstinct) where

import Arkham.Action qualified as Action
import Arkham.Helpers.Location
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype SurvivalInstinct = SurvivalInstinct SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

survivalInstinct :: SkillCard SurvivalInstinct
survivalInstinct = skill SurvivalInstinct Cards.survivalInstinct

instance RunMessage SurvivalInstinct where
  runMessage msg s@(SurvivalInstinct attrs) = runQueueT $ case msg of
    PassedSkillTest iid (Just Action.Evade) _ (isTarget attrs -> True) _ _ -> do
      enemies <- select $ enemyEngagedWith iid
      canDisengage <- iid <=~> InvestigatorCanDisengage
      locations <- getConnectedMoveLocations iid (toSource attrs)

      when ((notNull enemies && canDisengage) || notNull locations) do
        skillTestCardOption attrs do
          when (notNull enemies && canDisengage) do
            chooseOneM iid do
              cardI18n (scope "survivalInstinct" $ labeled' "disengage") $ for_ enemies (disengageEnemy iid)
              labeledI "skip" nothing

          chooseOrRunOneM iid do
            labeledI "doNotMoveToConnecting" nothing
            targets locations (moveTo attrs iid)
      pure s
    _ -> SurvivalInstinct <$> liftRunMessage msg attrs

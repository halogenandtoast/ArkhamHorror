module Arkham.Skill.Cards.SurvivalInstinct2 (survivalInstinct2) where

import Arkham.Action qualified as Action
import Arkham.Helpers.Location
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype SurvivalInstinct2 = SurvivalInstinct2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

survivalInstinct2 :: SkillCard SurvivalInstinct2
survivalInstinct2 = skill SurvivalInstinct2 Cards.survivalInstinct2

instance RunMessage SurvivalInstinct2 where
  runMessage msg s@(SurvivalInstinct2 attrs) = runQueueT $ case msg of
    PassedSkillTest iid (Just Action.Evade) _ (isTarget attrs -> True) _ _ -> do
      enemies <- select EnemyEngagedWithYou

      unless (null enemies) do
        chooseOneM iid do
          labeled "Evade each other enemy" do
            for_ enemies (push . EnemyEvaded iid)
          labeled "Skip" nothing

      locations <- getAccessibleLocations iid attrs
      unless (null locations) $ chooseOrRunOneM iid do
        labeled "Do not move to a connecting location" nothing
        targets locations (moveTo attrs iid)
      pure s
    _ -> SurvivalInstinct2 <$> liftRunMessage msg attrs

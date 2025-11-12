module Arkham.Story.Cards.IfTheUniformFits (ifTheUniformFits) where

import Arkham.Ability
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.ScenarioLogKey
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Arkham.SkillTest.Base
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype IfTheUniformFits = IfTheUniformFits StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ifTheUniformFits :: StoryCard IfTheUniformFits
ifTheUniformFits = story IfTheUniformFits Cards.ifTheUniformFits

instance HasAbilities IfTheUniformFits where
  getAbilities (IfTheUniformFits a) = [restricted a 1 (OnSameLocation <> criteria) fightAction_]
   where
    criteria = case a.placement of
      AttachedToEnemy e -> exists (EnemyWithId e <> CanFightEnemy (a.ability 1))
      _ -> Never

instance RunMessage IfTheUniformFits where
  runMessage msg s@(IfTheUniformFits attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      case attrs.placement of
        AttachedToEnemy e -> do
          sid <- getRandom
          beginSkillTestEdit sid iid (attrs.ability 1) e #combat (Fixed 3) \st -> st {skillTestAction = Just #fight}
        _ -> pure ()
      pure s
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      remember ObtainedAnEmployeeUniform
      case attrs.placement of
        AttachedToEnemy e -> do
          shouldRaise <-
            selectAny $ EnemyAt (orConnected_ (locationWithInvestigator iid)) <> not_ (EnemyWithId e)
          when shouldRaise $ raiseAlarmLevel (attrs.ability 1) [iid]
          toDiscardBy iid (attrs.ability 1) e
        _ -> pure ()
      pure s
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      raiseAlarmLevel (attrs.ability 1) [iid]
      pure s
    _ -> IfTheUniformFits <$> liftRunMessage msg attrs

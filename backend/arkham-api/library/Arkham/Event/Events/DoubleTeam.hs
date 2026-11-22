module Arkham.Event.Events.DoubleTeam (doubleTeam) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window (windowSkillTestId)
import Arkham.Helpers.Window.Enemy (attackedEnemy, attackingInvestigator)
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Modifier

newtype DoubleTeam = DoubleTeam EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

doubleTeam :: EventCard DoubleTeam
doubleTeam = event DoubleTeam Cards.doubleTeam

instance RunMessage DoubleTeam where
  runMessage msg e@(DoubleTeam attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid (is attrs -> True) _ ws _ -> do
      let sid = windowSkillTestId ws
      {- "Add your [agility] to the performing investigator's skill value": the
      modifier lands on the PERFORMER and names the Double Team player as the
      stat's owner, which is what 'AddSkillValueOf' is for. The window is raised
      ahead of the test, the last point a skill value can still be added. -}
      skillTestModifier sid attrs (attackingInvestigator ws) (AddSkillValueOf #agility iid)
      onSucceedByEffect sid (atLeast 2) attrs sid $ doStep 1 msg
      pure e
    DoStep 1 (InvestigatorPlayEvent _ (is attrs -> True) _ ws _) -> do
      -- The PERFORMER evades: it is their attack, and an automatic evasion
      -- disengages the enemy from whoever evades it.
      automaticallyEvadeEnemy (attackingInvestigator ws) (attackedEnemy ws)
      pure e
    _ -> DoubleTeam <$> liftRunMessage msg attrs

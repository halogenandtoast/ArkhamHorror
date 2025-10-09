module Arkham.Event.Events.BumsRush (bumsRush) where

import Arkham.Action qualified as Action
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message.Lifted.Move
import Arkham.Modifier

newtype BumsRush = BumsRush EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bumsRush :: EventCard BumsRush
bumsRush = event BumsRush Cards.bumsRush

instance RunMessage BumsRush where
  runMessage msg e@(BumsRush attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (AddSkillValue #combat)
      chooseEvadeEnemyEdit sid iid attrs (setTarget attrs)
      doStep 2 msg
      pure e
    ChosenEvadeEnemy _sid (isSource attrs -> True) eid -> do
      pure $ overAttrs (targetL ?~ toTarget eid) e
    Successful (Action.Evade, EnemyTarget eid) iid _ (isTarget attrs -> True) n -> do
      push $ EnemyEvaded iid eid
      when (n >= 2) $ nonAttackEnemyDamage (Just iid) attrs 1 eid
      pure e
    DoStep 2 (PlayThisEvent iid (is attrs -> True)) -> do
      for_ attrs.target.enemy \enemyId -> do
        -- because this is delayed, the enemy might have been defeated and we should not move it
        whenM (matches enemyId (InPlayEnemy NonEliteEnemy)) do
          choices <- select $ connectedFrom (locationWithInvestigator iid) <> LocationCanBeEnteredBy enemyId
          chooseOrRunOneM iid do
            labeled "Do not move enemy" nothing
            targets choices $ enemyMoveTo attrs enemyId
      pure e
    _ -> BumsRush <$> liftRunMessage msg attrs

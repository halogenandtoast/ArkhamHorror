module Arkham.Event.Events.BumsRush (bumsRush) where

import Arkham.Action qualified as Action
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Message.Lifted.Move
import Arkham.Matcher hiding (EnemyEvaded)
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
      pure e
    Successful (Action.Evade, EnemyTarget eid) iid _ (isTarget attrs -> True) n -> do
      push $ EnemyEvaded iid eid
      when (n >= 2) $ nonAttackEnemyDamage (Just iid) attrs 1 eid
      isElite <- eid <=~> EliteEnemy
      unless isElite $ afterSkillTest iid "Bum Rush" $ push $ WillMoveEnemy eid msg
      pure e
    WillMoveEnemy enemyId (Successful (Action.Evade, _) iid _ target _) | isTarget attrs target -> do
      choices <- select $ ConnectedFrom (locationWithInvestigator iid) <> LocationCanBeEnteredBy enemyId
      unless (null choices) do
        chooseOneM iid do
          labeled "Do not move enemy" nothing
          targets choices $ enemyMoveTo enemyId
      pure e
    _ -> BumsRush <$> liftRunMessage msg attrs

module Arkham.Event.Events.BaitAndSwitch (baitAndSwitch) where

import Arkham.Action qualified as Action
import Arkham.Event.Cards qualified as Cards (baitAndSwitch)
import Arkham.Event.Import.Lifted
import Arkham.ForMovement
import Arkham.Matcher hiding (EnemyEvaded)

newtype BaitAndSwitch = BaitAndSwitch EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baitAndSwitch :: EventCard BaitAndSwitch
baitAndSwitch = event BaitAndSwitch Cards.baitAndSwitch

instance RunMessage BaitAndSwitch where
  runMessage msg e@(BaitAndSwitch attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      chooseEvadeEnemyEdit sid iid attrs (setTarget attrs)
      pure e
    Successful (Action.Evade, EnemyTarget eid) iid _ (isTarget attrs -> True) _ -> do
      nonElite <- elem eid <$> select NonEliteEnemy
      pushAll $ EnemyEvaded iid eid : [WillMoveEnemy eid msg | nonElite]
      pure e
    WillMoveEnemy enemyId (Successful (Action.Evade, _) iid _ target _) | isTarget attrs target -> do
      choices <-
        select
          $ ConnectedFrom NotForMovement (locationWithInvestigator iid)
          <> LocationCanBeEnteredBy enemyId
      enemyMoveChoices <- capture $ chooseOne iid $ targetLabels choices $ only . EnemyMove enemyId
      insertAfterMatching enemyMoveChoices \case
        AfterEvadeEnemy {} -> True
        _ -> False
      pure e
    _ -> BaitAndSwitch <$> liftRunMessage msg attrs

module Arkham.Event.Events.CheatDeath5 (cheatDeath5) where

import Arkham.Classes.HasQueue (replaceMessageMatching)
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Helpers.Message qualified as Msg
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Strategy

newtype CheatDeath5 = CheatDeath5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cheatDeath5 :: EventCard CheatDeath5
cheatDeath5 = eventWith CheatDeath5 Cards.cheatDeath5 $ afterPlayL .~ RemoveThisFromGame

instance RunMessage CheatDeath5 where
  runMessage msg e@(CheatDeath5 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      lift $ replaceMessageMatching
        (\case InvestigatorWhenDefeated _ iid' -> iid == iid'; _ -> False)
        \case
          InvestigatorWhenDefeated source' _ -> [Msg.checkDefeated source' iid]
          _ -> error "invalid match"

      selectEach (enemyEngagedWith iid) (push . DisengageEnemy iid)
      selectEach (treacheryInThreatAreaOf iid) $ toDiscardBy iid attrs

      healHorrorIfCan iid attrs 2
      healDamageIfCan iid attrs 2

      locations <- getCanMoveToMatchingLocations iid attrs LocationWithoutEnemies
      when (notNull locations) do
        chooseOrRunOneM iid $ targets locations (moveTo (toSource attrs) iid)

      whenM (iid <=~> TurnInvestigator) $ push $ ChooseEndTurn iid
      pure e
    _ -> CheatDeath5 <$> liftRunMessage msg attrs

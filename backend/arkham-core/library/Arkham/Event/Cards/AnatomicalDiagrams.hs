module Arkham.Event.Cards.AnatomicalDiagrams (anatomicalDiagrams, AnatomicalDiagrams (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Game.Helpers
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Matcher

newtype AnatomicalDiagrams = AnatomicalDiagrams EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anatomicalDiagrams :: EventCard AnatomicalDiagrams
anatomicalDiagrams = event AnatomicalDiagrams Cards.anatomicalDiagrams

instance RunMessage AnatomicalDiagrams where
  runMessage msg e@(AnatomicalDiagrams attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      enemies <- select $ enemyAtLocationWith iid <> NonEliteEnemy
      selectOne TurnInvestigator >>= traverse_ \iid' -> do
        chooseOrRunOne
          iid
          [ targetLabel enemy [Msg.turnModifiers iid' attrs enemy [EnemyFight (-2), EnemyEvade (-2)]]
          | enemy <- enemies
          ]
      pure e
    _ -> AnatomicalDiagrams <$> liftRunMessage msg attrs

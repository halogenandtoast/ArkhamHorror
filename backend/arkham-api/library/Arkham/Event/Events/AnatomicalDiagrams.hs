module Arkham.Event.Events.AnatomicalDiagrams (anatomicalDiagrams, AnatomicalDiagrams (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

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
        chooseOrRunOneM iid do
          targets enemies \enemy -> turnModifiers iid' attrs enemy [EnemyFight (-2), EnemyEvade (-2)]
      pure e
    _ -> AnatomicalDiagrams <$> liftRunMessage msg attrs

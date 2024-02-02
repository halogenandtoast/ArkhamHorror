module Arkham.Event.Cards.Evidence1 (evidence1, Evidence1 (..)) where

import Arkham.Classes
import Arkham.Enemy.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.History
import Arkham.Location.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection

newtype Evidence1 = Evidence1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

evidence1 :: EventCard Evidence1
evidence1 = event Evidence1 Cards.evidence1

instance RunMessage Evidence1 where
  runMessage msg e@(Evidence1 attrs) = case msg of
    PlayThisEvent iid eid | eid == eventId attrs -> do
      enemiesDefeated <- historyEnemiesDefeated <$> getHistory TurnHistory iid
      totalPrintedHealth <-
        sum
          <$> traverse
            (getPlayerCountValue . fromMaybe (Static 0) . enemyHealth . defeatedEnemyAttrs)
            enemiesDefeated
      currentLocationId <- getJustLocation iid
      availableClues <- field LocationClues currentLocationId
      let amount = min availableClues (if totalPrintedHealth >= 4 then 2 else 1)
      pushAll
        $ [ InvestigatorDiscoverClues iid currentLocationId (toSource attrs) amount Nothing
          | amount > 0
          ]
      pure e
    _ -> Evidence1 <$> runMessage msg attrs

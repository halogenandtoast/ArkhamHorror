module Arkham.Event.Cards.DumbLuck2 (
  dumbLuck2,
  DumbLuck2 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Window

newtype DumbLuck2 = DumbLuck2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dumbLuck2 :: EventCard DumbLuck2
dumbLuck2 = event DumbLuck2 Cards.dumbLuck2

toEnemyId :: [Window] -> EnemyId
toEnemyId = go . map windowType
 where
  go = \case
    [] -> error "invalid use of card"
    (FailEvadeEnemy _ eid _ : _) -> eid
    (_ : xs) -> go xs

instance RunMessage DumbLuck2 where
  runMessage msg e@(DumbLuck2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ windows _ | eid == toId attrs -> do
      let enemyId = toEnemyId windows
      e
        <$ pushAll
          [ PutOnBottomOfDeck iid Deck.EncounterDeck (EnemyTarget enemyId)
          ]
    _ -> DumbLuck2 <$> runMessage msg attrs

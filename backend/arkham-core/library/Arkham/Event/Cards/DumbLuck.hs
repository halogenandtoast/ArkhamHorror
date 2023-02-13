module Arkham.Event.Cards.DumbLuck
  ( dumbLuck
  , DumbLuck(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Message
import Arkham.Target
import Arkham.Window

newtype DumbLuck = DumbLuck EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dumbLuck :: EventCard DumbLuck
dumbLuck = event DumbLuck Cards.dumbLuck

toEnemyId :: [Window] -> EnemyId
toEnemyId = go . map windowType
 where
  go = \case
    [] -> error "invalid use of card"
    (FailEvadeEnemy _ eid _ : _) -> eid
    (_ : xs) -> go xs

instance RunMessage DumbLuck where
  runMessage msg e@(DumbLuck attrs) = case msg of
    InvestigatorPlayEvent iid eid _ windows _ | eid == toId attrs -> do
      let enemyId = toEnemyId windows
      e <$ pushAll
        [ PutOnTopOfDeck iid Deck.EncounterDeck (EnemyTarget enemyId)
        ]
    _ -> DumbLuck <$> runMessage msg attrs

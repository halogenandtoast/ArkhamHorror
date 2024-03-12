module Arkham.Event.Cards.BellyOfTheBeast (bellyOfTheBeast, BellyOfTheBeast (..)) where

import Arkham.Classes
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype BellyOfTheBeast = BellyOfTheBeast EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bellyOfTheBeast :: EventCard BellyOfTheBeast
bellyOfTheBeast = event BellyOfTheBeast Cards.bellyOfTheBeast

toEnemyId :: [Window] -> EnemyId
toEnemyId [] = error "Invalid window"
toEnemyId ((windowType -> Window.SuccessfulEvadeEnemy _ enemyId _) : _) = enemyId
toEnemyId (_ : xs) = toEnemyId xs

instance RunMessage BellyOfTheBeast where
  runMessage msg e@(BellyOfTheBeast attrs) = case msg of
    InvestigatorPlayEvent iid eid _ (toEnemyId -> enemyId) _ | eid == toId attrs -> do
      mlid <- selectOne $ locationWithEnemy enemyId
      for_ mlid $ \lid -> push $ toMessage $ discover iid lid attrs 1
      pure e
    _ -> BellyOfTheBeast <$> runMessage msg attrs

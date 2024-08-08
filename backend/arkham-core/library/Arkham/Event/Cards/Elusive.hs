module Arkham.Event.Cards.Elusive where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Movement
import Arkham.Prelude
import Arkham.Taboo

newtype Elusive = Elusive EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elusive :: EventCard Elusive
elusive = event Elusive Cards.elusive

instance RunMessage Elusive where
  runMessage msg e@(Elusive attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      enemies <- select $ enemyEngagedWith iid
      targets <-
        getCanMoveToMatchingLocations iid attrs
          $ if tabooed TabooList19 attrs
            then LocationWithoutEnemies <> AccessibleFrom (locationWithInvestigator iid)
            else LocationWithoutEnemies <> RevealedLocation
      player <- getPlayer iid
      pushAll
        $ map (DisengageEnemy iid) enemies
        <> [chooseOrRunOne player $ targetLabels targets (only . MoveTo . move attrs iid) | notNull targets]
        <> map EnemyCheckEngagement enemies
      pure e
    _ -> Elusive <$> runMessage msg attrs

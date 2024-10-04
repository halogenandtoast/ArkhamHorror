module Arkham.Event.Events.Elusive where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Movement
import Arkham.Taboo

newtype Elusive = Elusive EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elusive :: EventCard Elusive
elusive = event Elusive Cards.elusive

instance RunMessage Elusive where
  runMessage msg e@(Elusive attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      enemies <- select $ enemyEngagedWith iid
      ts <-
        getCanMoveToMatchingLocations
          iid
          attrs
          $ LocationWithoutEnemies
          <> if tabooed TabooList19 attrs
            then AccessibleFrom (locationWithInvestigator iid)
            else RevealedLocation
      for_ enemies $ disengageEnemy iid
      when (notNull ts) do
        chooseOrRunOne iid $ targetLabels ts (only . Move . move attrs iid)
      for_ enemies enemyCheckEngagement
      pure e
    _ -> Elusive <$> liftRunMessage msg attrs

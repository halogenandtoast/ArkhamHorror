module Arkham.Event.Cards.WarningShot (warningShot, WarningShot (..)) where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype WarningShot = WarningShot EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

warningShot :: EventCard WarningShot
warningShot = event WarningShot Cards.warningShot

instance RunMessage WarningShot where
  runMessage msg e@(WarningShot attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      eids <- select $ NonEliteEnemy <> enemyAtLocationWith iid

      lids <-
        nub <$> concatForM eids \eid' -> do
          select
            $ ConnectedLocation
            <> LocationCanBeEnteredBy eid'
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [targetLabel lid (map (`EnemyMove` lid) eids) | lid <- lids]
      pure e
    _ -> WarningShot <$> runMessage msg attrs

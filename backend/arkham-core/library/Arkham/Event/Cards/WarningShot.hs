module Arkham.Event.Cards.WarningShot (
  warningShot,
  WarningShot (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher

newtype WarningShot = WarningShot EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

warningShot :: EventCard WarningShot
warningShot = event WarningShot Cards.warningShot

instance RunMessage WarningShot where
  runMessage msg e@(WarningShot attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      eids <-
        selectList
          $ NonEliteEnemy
          <> EnemyAt
            (locationWithInvestigator iid)

      lids <-
        nub <$> concatForM eids \eid' -> do
          selectList
            $ ConnectedLocation
            <> LocationCanBeEnteredBy eid'
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [targetLabel lid (map (`EnemyMove` lid) eids) | lid <- lids]
      pure e
    _ -> WarningShot <$> runMessage msg attrs

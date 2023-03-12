module Arkham.Agenda.Cards.FuryThatShakesTheEarth
  ( FuryThatShakesTheEarth(..)
  , furyThatShakesTheEarth
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Criteria
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenarios.TheDepthsOfYoth.Helpers
import Arkham.Timing qualified as Timing

newtype FuryThatShakesTheEarth = FuryThatShakesTheEarth AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

furyThatShakesTheEarth :: AgendaCard FuryThatShakesTheEarth
furyThatShakesTheEarth =
  agenda (5, A) FuryThatShakesTheEarth Cards.furyThatShakesTheEarth (Static 5)

-- We limit this ability, even though it isn't limited because we don't have a
-- good way to track when it goes from less than 3 to 3 in the case that
-- multiple counters get placed. By limiting it we trigger once it is at or
-- past 3, but never gain.
instance HasAbilities FuryThatShakesTheEarth where
  getAbilities (FuryThatShakesTheEarth a) =
    [ limitedAbility (GroupLimit PerGame 1)
        $ restrictedAbility
            a
            1
            (AgendaExists $ AgendaWithId (toId a) <> AgendaWithDoom
              (AtLeast $ Static 3)
            )
        $ ForcedAbility
        $ PlacedCounterOnAgenda
            Timing.After
            (AgendaWithSide A)
            DoomCounter
            (AtLeast $ Static 1)
    ]

instance RunMessage FuryThatShakesTheEarth where
  runMessage msg a@(FuryThatShakesTheEarth attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      enemyMsgs <- getPlacePursuitEnemyMessages
      mYig <- maybeGetSetAsideEncounterCard Enemies.yig
      depthStart <- getDepthStart
      yigMsgs <- for (toList mYig)
        $ \yig -> createEnemyAt_ (EncounterCard yig) depthStart Nothing
      pushAll
        $ enemyMsgs
        <> yigMsgs
        <> [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      pushAllM getPlacePursuitEnemyMessages
      pure a
    _ -> FuryThatShakesTheEarth <$> runMessage msg attrs

module Arkham.Agenda.Cards.FuryThatShakesTheEarth (furyThatShakesTheEarth) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Scenarios.TheDepthsOfYoth.Helpers

newtype FuryThatShakesTheEarth = FuryThatShakesTheEarth AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

furyThatShakesTheEarth :: AgendaCard FuryThatShakesTheEarth
furyThatShakesTheEarth = agenda (5, A) FuryThatShakesTheEarth Cards.furyThatShakesTheEarth (Static 5)

-- We limit this ability, even though it isn't limited because we don't have a
-- good way to track when it goes from less than 3 to 3 in the case that
-- multiple counters get placed. By limiting it we trigger once it is at or
-- past 3, but never gain.
instance HasAbilities FuryThatShakesTheEarth where
  getAbilities (FuryThatShakesTheEarth a) =
    [ groupLimit PerGame
        $ restricted a 1 (thisExists a $ AgendaWithDoom (AtLeast $ Static 3))
        $ forced
        $ PlacedCounterOnAgenda #after (AgendaWithSide A) AnySource DoomCounter (AtLeast $ Static 1)
    ]

instance RunMessage FuryThatShakesTheEarth where
  runMessage msg a@(FuryThatShakesTheEarth attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      placePursuitEnemies
      mYig <- maybeGetSetAsideEncounterCard Enemies.yig
      depthStart <- getDepthStart
      for_ mYig (`createEnemyAt_` depthStart)
      advanceAgendaDeck attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placePursuitEnemies
      pure a
    _ -> FuryThatShakesTheEarth <$> liftRunMessage msg attrs

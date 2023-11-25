module Arkham.Agenda.Cards.TheInfestationSpreads (
  TheInfestationSpreads (..),
  theInfestationSpreads,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Scenarios.WakingNightmare.Helpers

newtype TheInfestationSpreads = TheInfestationSpreads AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theInfestationSpreads :: AgendaCard TheInfestationSpreads
theInfestationSpreads = agenda (2, A) TheInfestationSpreads Cards.theInfestationSpreads (Static 6)

instance HasAbilities TheInfestationSpreads where
  getAbilities (TheInfestationSpreads attrs) =
    [mkAbility attrs 1 $ ForcedAbility $ PhaseEnds #when #mythos]

instance RunMessage TheInfestationSpreads where
  runMessage msg a@(TheInfestationSpreads attrs) =
    case msg of
      UseThisAbility _ (isSource attrs -> True) 1 -> do
        makeInfestationTest
        pure a
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheInfestationSpreads <$> runMessage msg attrs

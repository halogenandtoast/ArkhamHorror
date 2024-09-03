module Arkham.Agenda.Cards.ASinisterRealm (ASinisterRealm (..), aSinisterRealm) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Prelude
import Arkham.ScenarioLogKey

newtype ASinisterRealm = ASinisterRealm AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aSinisterRealm :: AgendaCard ASinisterRealm
aSinisterRealm = agenda (1, A) ASinisterRealm Cards.aSinisterRealm (Static 5)

instance RunMessage ASinisterRealm where
  runMessage msg a@(ASinisterRealm attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
        pushAll
          [ShuffleEncounterDiscardBackIn, ScenarioCountIncrementBy Distortion 1, advanceAgendaDeck attrs]
        pure a
      _ -> ASinisterRealm <$> runMessage msg attrs

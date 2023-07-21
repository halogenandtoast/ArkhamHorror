module Arkham.Agenda.Cards.CallingForthTheOldOnes (
  CallingForthTheOldOnes (..),
  callingForthTheOldOnes,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Agenda.Types
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype CallingForthTheOldOnes = CallingForthTheOldOnes AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callingForthTheOldOnes :: AgendaCard CallingForthTheOldOnes
callingForthTheOldOnes =
  agenda (1, A) CallingForthTheOldOnes Cards.callingForthTheOldOnes (Static 12)

instance RunMessage CallingForthTheOldOnes where
  runMessage msg a@(CallingForthTheOldOnes attrs@AgendaAttrs {..}) =
    case msg of
      AdvanceAgenda aid
        | aid == agendaId && onSide B attrs ->
            a
              <$ pushAll
                [ ShuffleEncounterDiscardBackIn
                , AdvanceAgendaDeck agendaDeckId (toSource attrs)
                ]
      _ -> CallingForthTheOldOnes <$> runMessage msg attrs

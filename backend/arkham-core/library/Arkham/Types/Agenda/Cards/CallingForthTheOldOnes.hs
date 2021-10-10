module Arkham.Types.Agenda.Cards.CallingForthTheOldOnes
  ( CallingForthTheOldOnes
  , callingForthTheOldOnes
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message

newtype CallingForthTheOldOnes = CallingForthTheOldOnes AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callingForthTheOldOnes :: AgendaCard CallingForthTheOldOnes
callingForthTheOldOnes =
  agenda (1, A) CallingForthTheOldOnes Cards.callingForthTheOldOnes (Static 12)

instance AgendaRunner env => RunMessage env CallingForthTheOldOnes where
  runMessage msg a@(CallingForthTheOldOnes attrs@AgendaAttrs {..}) =
    case msg of
      AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 1 B ->
        a <$ pushAll
          [ ShuffleEncounterDiscardBackIn
          , AdvanceAgendaDeck agendaDeckId (toSource attrs)
          ]
      _ -> CallingForthTheOldOnes <$> runMessage msg attrs

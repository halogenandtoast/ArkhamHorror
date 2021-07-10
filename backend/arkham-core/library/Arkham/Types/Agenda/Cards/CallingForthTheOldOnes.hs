module Arkham.Types.Agenda.Cards.CallingForthTheOldOnes
  ( CallingForthTheOldOnes
  , callingForthTheOldOnes
  ) where

import Arkham.Prelude

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message

newtype CallingForthTheOldOnes = CallingForthTheOldOnes AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callingForthTheOldOnes :: CallingForthTheOldOnes
callingForthTheOldOnes = CallingForthTheOldOnes
  $ baseAttrs "02275" "Calling Forth the Old Ones" (Agenda 1 A) (Static 12)

instance HasModifiersFor env CallingForthTheOldOnes

instance HasActions env CallingForthTheOldOnes where
  getActions i window (CallingForthTheOldOnes x) = getActions i window x

instance AgendaRunner env => RunMessage env CallingForthTheOldOnes where
  runMessage msg a@(CallingForthTheOldOnes attrs@AgendaAttrs {..}) =
    case msg of
      AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 1 B ->
        a <$ pushAll [ShuffleEncounterDiscardBackIn, NextAgenda aid "02276"]
      _ -> CallingForthTheOldOnes <$> runMessage msg attrs

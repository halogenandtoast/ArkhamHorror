module Arkham.Agenda.Sequence where

import Arkham.Prelude

newtype AgendaStep = AgendaStep { unAgendaStep :: Int }
  deriving newtype Eq

agendaStep :: AgendaSequence -> AgendaStep
agendaStep (Sequence num _) = AgendaStep num

agendaSide :: AgendaSequence -> AgendaSide
agendaSide (Sequence _ side) = side

data AgendaSide = A | B | C | D
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data AgendaSequence = Sequence Int AgendaSide
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

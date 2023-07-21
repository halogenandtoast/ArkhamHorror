{-# LANGUAGE TemplateHaskell #-}

module Arkham.Agenda.Sequence where

import Arkham.Prelude

import Data.Aeson.TH

newtype AgendaStep = AgendaStep {unAgendaStep :: Int}
  deriving newtype (Eq)

agendaStep :: AgendaSequence -> AgendaStep
agendaStep (Sequence num _) = AgendaStep num

agendaSide :: AgendaSequence -> AgendaSide
agendaSide (Sequence _ side) = side

data AgendaSide = A | B | C | D
  deriving stock (Eq, Show, Ord)

data AgendaSequence = Sequence
  { agendaSequenceStep :: Int
  , agendaSequenceSide :: AgendaSide
  }
  deriving stock (Eq, Show, Ord)

flipAgendaSide :: AgendaSide -> AgendaSide
flipAgendaSide = \case
  A -> B
  B -> A
  C -> D
  D -> C

flipSequence :: AgendaSequence -> AgendaSequence
flipSequence as =
  as {agendaSequenceSide = flipAgendaSide (agendaSequenceSide as)}

$(deriveJSON defaultOptions ''AgendaSide)
$(deriveJSON defaultOptions ''AgendaSequence)

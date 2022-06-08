{-# LANGUAGE TemplateHaskell #-}

module Arkham.Agenda (
  module Arkham.Agenda,
) where

import Arkham.Prelude

import Arkham.Agenda.Agendas
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Id
import Data.Aeson.TH

$(buildEntity "Agenda")
$(deriveJSON defaultOptions ''Agenda)

lookupAgenda :: AgendaId -> (Int -> Agenda)
lookupAgenda agendaId =
  fromJustNote ("Unknown agenda: " <> show agendaId) $
    lookup agendaId allAgendas

allAgendas :: HashMap AgendaId (Int -> Agenda)
allAgendas =
  mapFromList $
    map
      (\cb -> (AgendaId (cbCardCode cb), \deckId -> cbCardBuilder cb (deckId, AgendaId (cbCardCode cb))))
      $(buildEntityLookupList "Agenda")

instance HasAbilities Agenda where
  getAbilities = $(entityF "Agenda" "getAbilities")

instance RunMessage Agenda where
  runMessage = $(entityRunMessage "Agenda")

instance HasModifiersFor Agenda where
  getModifiersFor = $(entityF2 "Agenda" "getModifiersFor")

instance Entity Agenda where
  type EntityId Agenda = AgendaId
  type EntityAttrs Agenda = AgendaAttrs
  toId = toId . toAttrs
  toAttrs = $(entityF "Agenda" "toAttrs")

instance TargetEntity Agenda where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Agenda where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

{-# LANGUAGE TemplateHaskell #-}

module Arkham.Agenda (
  module Arkham.Agenda,
) where

import Arkham.Prelude

import Arkham.Agenda.Agendas
import Arkham.Agenda.Attrs
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Id
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Name
import Arkham.Query
import Arkham.Trait (Trait)

$(buildEntity "Agenda")

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

instance HasList UnderneathCard env Agenda where
  getList = getList . toAttrs

instance HasModifiersFor env () => HasCount DoomCount env Agenda where
  getCount a = do
    modifiers <- getModifiers (toSource a) (toTarget a)
    let f = if DoomSubtracts `elem` modifiers then negate else id
    DoomCount . f . unDoomCount <$> getCount (toAttrs a)

instance HasStep AgendaStep env Agenda where
  getStep = getStep . toAttrs

instance HasAbilities Agenda where
  getAbilities = $(entityF "Agenda" "getAbilities")

instance (HasId (Maybe EnemyId) env EnemyMatcher, AgendaRunner env) => RunMessage env Agenda where
  runMessage = $(entityRunMessage "Agenda")

instance (Query EnemyMatcher env, HasSet Trait env EnemyId, HasRecord env ()) => HasModifiersFor env Agenda where
  getModifiersFor = $(entityF2 "Agenda" "getModifiersFor")

instance Entity Agenda where
  type EntityId Agenda = AgendaId
  type EntityAttrs Agenda = AgendaAttrs
  toId = toId . toAttrs
  toAttrs = $(entityF "Agenda" "toAttrs")

instance Named Agenda where
  toName = toName . toAttrs

instance TargetEntity Agenda where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Agenda where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

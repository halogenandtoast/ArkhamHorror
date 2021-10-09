{-# LANGUAGE TemplateHaskell #-}
module Arkham.Types.Agenda
  ( module Arkham.Types.Agenda
  ) where

import Arkham.Prelude

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Cards
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.Trait (Trait)

$(buildEntity "Agenda")

lookupAgenda :: AgendaId -> (Int -> Agenda)
lookupAgenda agendaId =
  fromJustNote ("Unknown agenda: " <> show agendaId)
    $ lookup agendaId allAgendas

allAgendas :: HashMap AgendaId (Int -> Agenda)
allAgendas = mapFromList $ map
  (\cb -> (AgendaId (cbCardCode cb), \deckId -> cbCardBuilder cb (deckId, AgendaId (cbCardCode cb))))
  $(buildEntityLookupList "Agenda")

instance HasList UnderneathCard env Agenda where
  getList = getList . toAttrs

instance HasCount DoomCount env Agenda where
  getCount = getCount . toAttrs

instance HasStep AgendaStep env Agenda where
  getStep = getStep . toAttrs

instance HasAbilities Agenda where
  getAbilities = genericGetAbilities

instance (HasId (Maybe EnemyId) env EnemyMatcher, AgendaRunner env) => RunMessage env Agenda where
  runMessage = genericRunMessage

instance (Query EnemyMatcher env, HasSet Trait env EnemyId) => HasModifiersFor env Agenda where
  getModifiersFor = genericGetModifiersFor

instance Entity Agenda where
  type EntityId Agenda = AgendaId
  type EntityAttrs Agenda = AgendaAttrs

instance Named Agenda where
  toName = toName . toAttrs

instance TargetEntity Agenda where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Agenda where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

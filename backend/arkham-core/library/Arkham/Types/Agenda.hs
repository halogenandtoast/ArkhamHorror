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

lookupAgenda :: AgendaId -> Agenda
lookupAgenda agendaId =
  fromJustNote ("Unknown agenda: " <> show agendaId)
    $ lookup agendaId allAgendas

allAgendas :: Map AgendaId Agenda
allAgendas = mapFromList $ map
  (\cb -> (AgendaId (cbCardCode cb), cbCardBuilder cb (AgendaId (cbCardCode cb))))
  $(buildEntityLookupList "Agenda")

instance HasList UnderneathCard env Agenda where
  getList = getList . toAttrs

instance HasCount DoomCount env Agenda where
  getCount = getCount . toAttrs

instance HasStep Agenda AgendaStep where
  getStep = ask >>= runReaderT getStep . toAttrs

deriving anyclass instance ActionRunner env => HasActions env Agenda

instance (HasId (Maybe EnemyId) env EnemyMatcher, HasRecord env, AgendaRunner env) => RunMessage env Agenda where
  runMessage = genericRunMessage

instance (HasSet EnemyId env (), HasSet Trait env EnemyId) => HasModifiersFor env Agenda where
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

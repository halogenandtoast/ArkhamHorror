module Arkham.Helpers.Agenda where

import Arkham.Prelude

import Arkham.Agenda.Attrs (Field(..))
import Arkham.Agenda.Sequence qualified as AS
import Arkham.AgendaId
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.GameEnv
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Matcher
import Arkham.Projection

getCurrentAgendaStep :: GameT Int
getCurrentAgendaStep = selectJust AnyAgenda >>= getAgendaStep

getAgendaStep :: AgendaId -> GameT Int
getAgendaStep = fieldMap AgendaSequence (unAgendaStep . AS.agendaStep)

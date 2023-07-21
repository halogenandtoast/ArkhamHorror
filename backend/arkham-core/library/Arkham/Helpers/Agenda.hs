module Arkham.Helpers.Agenda where

import Arkham.Prelude

import Arkham.Agenda.Sequence qualified as AS
import Arkham.Agenda.Types (Field (..))
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Matcher
import Arkham.Projection

getCurrentAgendaStep :: HasGame m => m Int
getCurrentAgendaStep = getCurrentAgenda >>= getAgendaStep

getAgendaStep :: HasGame m => AgendaId -> m Int
getAgendaStep = fieldMap AgendaSequence (AS.unAgendaStep . AS.agendaStep)

getCurrentAgenda :: HasGame m => m AgendaId
getCurrentAgenda = selectOnlyOne AnyAgenda

module Arkham.Helpers.Agenda where

import Arkham.Prelude

import Arkham.Agenda.Types (Field(..))
import Arkham.Agenda.Sequence qualified as AS
import Arkham.Id
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.GameEnv
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Matcher
import Arkham.Projection

getCurrentAgendaStep :: (Monad m, HasGame m) => m Int
getCurrentAgendaStep = selectJust AnyAgenda >>= getAgendaStep

getAgendaStep :: (Monad m, HasGame m) => AgendaId -> m Int
getAgendaStep = fieldMap AgendaSequence (AS.unAgendaStep . AS.agendaStep)

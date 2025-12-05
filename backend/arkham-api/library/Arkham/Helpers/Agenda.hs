module Arkham.Helpers.Agenda where

import Arkham.Agenda.Sequence qualified as AS
import Arkham.Agenda.Types (Field (..))
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Tracing

currentAgendaSequenceIs :: (Tracing m, HasGame m) => (AS.AgendaSequence -> Bool) -> m Bool
currentAgendaSequenceIs f = getCurrentAgenda >>= fieldMap AgendaSequence f

currentAgendaStepIs :: (Tracing m, HasGame m) => (Int -> Bool) -> m Bool
currentAgendaStepIs f = f <$> getCurrentAgendaStep

whenCurrentAgendaStepIs :: (Tracing m, HasGame m) => (Int -> Bool) -> m () -> m ()
whenCurrentAgendaStepIs f = whenM (f <$> getCurrentAgendaStep)

getCurrentAgendaStep :: (Tracing m, HasGame m) => m Int
getCurrentAgendaStep = getCurrentAgenda >>= getAgendaStep

getAgendaStep :: (HasGame m, Tracing m) => AgendaId -> m Int
getAgendaStep = fieldMap AgendaSequence (AS.unAgendaStep . AS.agendaStep)

getCurrentAgenda :: (Tracing m, HasGame m) => m AgendaId
getCurrentAgenda = selectOnlyOne AnyAgenda

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

currentAgendaSequenceIs :: HasGame m => (AS.AgendaSequence -> Bool) -> m Bool
currentAgendaSequenceIs f = getCurrentAgenda >>= fieldMap AgendaSequence f

currentAgendaStepIs :: HasGame m => (Int -> Bool) -> m Bool
currentAgendaStepIs f = f <$> getCurrentAgendaStep

whenCurrentAgendaStepIs :: HasGame m => (Int -> Bool) -> m () -> m ()
whenCurrentAgendaStepIs f = whenM (f <$> getCurrentAgendaStep)

getCurrentAgendaStep :: HasGame m => m Int
getCurrentAgendaStep = getCurrentAgenda >>= getAgendaStep

getAgendaStep :: HasGame m => AgendaId -> m Int
getAgendaStep = fieldMap AgendaSequence (AS.unAgendaStep . AS.agendaStep)

getCurrentAgenda :: HasGame m => m AgendaId
getCurrentAgenda = selectOnlyOne AnyAgenda

getDoomOnAgenda :: HasGame m => m Int
getDoomOnAgenda = selectJust AnyAgenda >>= field AgendaDoom

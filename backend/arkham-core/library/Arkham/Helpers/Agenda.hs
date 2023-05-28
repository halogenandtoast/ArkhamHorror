module Arkham.Helpers.Agenda where

import Arkham.Prelude

import Arkham.Agenda.Sequence qualified as AS
import Arkham.Agenda.Types (Field (..))
import Arkham.Card
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Matcher
import Arkham.Projection
import Arkham.Store

getCurrentAgendaStep :: (Store m Card, HasGame m) => m Int
getCurrentAgendaStep = selectJust AnyAgenda >>= getAgendaStep

getAgendaStep :: (HasGame m, Store m Card) => AgendaId -> m Int
getAgendaStep = fieldMap AgendaSequence (AS.unAgendaStep . AS.agendaStep)

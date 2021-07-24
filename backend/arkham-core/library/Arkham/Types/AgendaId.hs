module Arkham.Types.AgendaId where

import Arkham.Prelude

import Arkham.Types.Card.CardCode

newtype AgendaStep = AgendaStep { unAgendaStep :: Int }

newtype AgendaId = AgendaId { unAgendaId :: CardCode }
  deriving newtype (Eq, Hashable, Show, ToJSON, FromJSON, IsString, ToJSONKey, FromJSONKey)

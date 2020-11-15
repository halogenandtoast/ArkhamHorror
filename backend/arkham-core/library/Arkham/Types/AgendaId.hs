module Arkham.Types.AgendaId where

import Arkham.Types.Card.CardCode
import ClassyPrelude
import Data.Aeson

newtype AgendaStep = AgendaStep { unAgendaStep :: Int }

newtype AgendaId = AgendaId CardCode
  deriving newtype (Eq, Hashable, Show, ToJSON, FromJSON, IsString, ToJSONKey, FromJSONKey)

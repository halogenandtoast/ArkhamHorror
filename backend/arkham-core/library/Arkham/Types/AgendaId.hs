module Arkham.Types.AgendaId where

import Arkham.Types.Card.CardCode
import ClassyPrelude
import Data.Aeson

newtype AgendaId = AgendaId CardCode
  deriving newtype (Eq, Hashable, Show, ToJSON, FromJSON, IsString, ToJSONKey, FromJSONKey)

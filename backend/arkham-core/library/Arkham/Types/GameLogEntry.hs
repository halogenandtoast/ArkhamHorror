module Arkham.Types.GameLogEntry where

import Arkham.Json
import ClassyPrelude

newtype GameLogEntry = GameLogEntry { unGameLogEntry :: Text }
  deriving newtype (Show, Eq, FromJSON, ToJSON)

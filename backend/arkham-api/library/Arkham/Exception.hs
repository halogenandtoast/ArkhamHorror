module Arkham.Exception (module Arkham.Exception) where

import Arkham.Card.CardCode
import Arkham.Prelude
import Arkham.Resolution

newtype InvalidState = InvalidState Text
  deriving stock Show
  deriving anyclass Exception

newtype MissingCard = MissingCard CardCode
  deriving stock Show
  deriving anyclass Exception

newtype UnknownResolution = UnknownResolution Resolution
  deriving stock Show
  deriving anyclass Exception

module Arkham.Types.Exception
  ( module Arkham.Types.Exception
  ) where

import Arkham.Prelude

import Arkham.Types.Card.CardCode
import Arkham.Types.Resolution

newtype InvalidState = InvalidState Text
  deriving stock (Typeable, Show)
  deriving anyclass Exception

newtype MissingCard = MissingCard CardCode
  deriving stock (Typeable, Show)
  deriving anyclass Exception

newtype UnknownResolution = UnknownResolution Resolution
  deriving stock (Typeable, Show)
  deriving anyclass Exception

module Arkham.Exception (
  module Arkham.Exception,
) where

import Arkham.Prelude

import Arkham.Card.CardCode
import Arkham.Resolution

newtype InvalidState = InvalidState Text
  deriving stock (Typeable, Show)
  deriving anyclass (Exception)

newtype MissingCard = MissingCard CardCode
  deriving stock (Typeable, Show)
  deriving anyclass (Exception)

newtype UnknownResolution = UnknownResolution Resolution
  deriving stock (Typeable, Show)
  deriving anyclass (Exception)

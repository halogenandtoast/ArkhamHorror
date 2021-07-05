module Arkham.Types.Exception
  ( module Arkham.Types.Exception
  ) where

import Arkham.Prelude

import Arkham.Types.Card.CardCode

newtype InvalidState = InvalidState Text
  deriving stock Typeable
  deriving anyclass Exception
  deriving newtype Show

newtype MissingCard = MissingCard CardCode
  deriving stock Typeable
  deriving anyclass Exception
  deriving newtype Show

module Arkham.Types.Exception
  ( module Arkham.Types.Exception
  ) where

import Arkham.Prelude

newtype InvalidState = InvalidState Text
  deriving stock Typeable
  deriving anyclass Exception
  deriving newtype Show

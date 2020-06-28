module Arkham.Constructors where

import Arkham.Internal.Types
import ClassyPrelude
import GHC.Stack

token :: HasCallStack => ArkhamChaosTokenInternal
token = ArkhamChaosTokenInternal
  { tokenToResult = error "you must specify a result"
  , tokenOnFail = const
  , tokenOnSuccess = const
  , tokenOnReveal = const
  }


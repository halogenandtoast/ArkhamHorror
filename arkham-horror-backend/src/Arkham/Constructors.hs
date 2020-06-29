module Arkham.Constructors
  ( token
  )
where

import Arkham.Internal.Types
import Arkham.Types.ChaosToken
import ClassyPrelude
import GHC.Stack

token :: HasCallStack => ArkhamChaosToken -> ArkhamChaosTokenInternal
token tokenType = ArkhamChaosTokenInternal
  { tokenToResult = error $ "you must specify a result for " <> show tokenType
  , tokenOnFail = const
  , tokenOnSuccess = const
  , tokenOnReveal = const
  }


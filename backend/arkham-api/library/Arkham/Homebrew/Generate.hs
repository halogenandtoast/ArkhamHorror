module Arkham.Homebrew.Generate (generateHomebrew) where

import Arkham.Homebrew.TH (discoverInstances)
import Arkham.Prelude
import Control.Monad.Fail (fail)
import Language.Haskell.TH (Exp, Q, lookupTypeName, lookupValueName)

-- | Generate the cards registered by a generated homebrew card-entry module.
generateHomebrew :: Q Exp
generateHomebrew = do
  cls <- lookupTypeName "IsHomebrewCard" >>= maybe (fail "IsHomebrewCard is not in scope") pure
  method <- lookupValueName "homebrewCard" >>= maybe (fail "homebrewCard is not in scope") pure
  discoverInstances cls method

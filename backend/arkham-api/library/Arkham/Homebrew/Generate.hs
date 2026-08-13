module Arkham.Homebrew.Generate (generateHomebrew, generateHomebrewCardDefs) where

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

{- | Generate the card definitions registered by a generated homebrew
card-def-entry module (see 'Arkham.Homebrew.DefsBase.IsHomebrewCardDefs').
-}
generateHomebrewCardDefs :: Q Exp
generateHomebrewCardDefs = do
  cls <-
    lookupTypeName "IsHomebrewCardDefs" >>= maybe (fail "IsHomebrewCardDefs is not in scope") pure
  method <-
    lookupValueName "homebrewCardDefs" >>= maybe (fail "homebrewCardDefs is not in scope") pure
  discoverInstances cls method

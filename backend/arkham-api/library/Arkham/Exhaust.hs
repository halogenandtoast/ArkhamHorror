{-# LANGUAGE TemplateHaskell #-}

module Arkham.Exhaust where

import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH
import GHC.Records

data Exhaustion msg = Exhaustion
  { exhaustionSource :: Source
  , exhaustionTarget :: Target
  , exhaustionThen :: [msg]
  }
  deriving stock (Show, Eq, Ord, Data, Functor, Foldable, Traversable)

instance HasField "source" (Exhaustion msg) Source where
  getField = exhaustionSource

instance HasField "target" (Exhaustion msg) Target where
  getField = exhaustionTarget

instance HasField "thenMsgs" (Exhaustion msg) [msg] where
  getField = exhaustionThen

mkExhaustion :: (Sourceable s, Targetable t) => s -> t -> Exhaustion msg
mkExhaustion s t = Exhaustion (toSource s) (toTarget t) []

mkExhaustionThen :: (Sourceable s, Targetable t) => s -> t -> [msg] -> Exhaustion msg
mkExhaustionThen s t msgs = Exhaustion (toSource s) (toTarget t) msgs

$(deriveJSON defaultOptions ''Exhaustion)

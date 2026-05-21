{-# LANGUAGE TemplateHaskell #-}

module Arkham.Message.Exhaust where

import Arkham.Exhaust
import {-# SOURCE #-} Arkham.Message (Message)
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH

-- | Messages dealing with exhausting and readying entities.
--
-- Re-exported by "Arkham.Message"; callers should use the bidirectional pattern
-- synonyms ('Exhaust', 'Ready', 'ReadyAlternative', 'ReadyExhausted') defined
-- there.
data ExhaustMessage
  = Exhaust_ (Exhaustion Message)
  | Ready_ Target
  | ReadyAlternative_ Source Target
  | ReadyExhausted_
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON defaultOptions ''ExhaustMessage)

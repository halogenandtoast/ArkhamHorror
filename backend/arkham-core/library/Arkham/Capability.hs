module Arkham.Capability where

import Arkham.Matcher.Patterns
import Arkham.Matcher.Types
import Data.Kind
import GHC.OverloadedLabels

class Capability a where
  data CapabilityInput a :: Type
  can :: a -> CapabilityInput a -> InvestigatorMatcher

data SearchCapability = SearchCapability

instance IsLabel "search" SearchCapability where
  fromLabel = SearchCapability

instance Capability SearchCapability where
  data CapabilityInput SearchCapability = Deck
  can _ = \case
    Deck -> InvestigatorCanSearchDeck

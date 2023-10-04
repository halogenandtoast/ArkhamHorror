{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Capability where

import Arkham.Matcher.Patterns
import Arkham.Matcher.Types
import Arkham.Modifier

can :: Capabilities
can =
  Capabilities
    { search = SearchCapabilities {deck = InvestigatorCanSearchDeck}
    , draw = DrawCapabilities {cards = InvestigatorWithoutModifier CannotDrawCards}
    , gain = GainCapabilities {resources = InvestigatorWithoutModifier CannotGainResources}
    }

data Capabilities = Capabilities
  { search :: SearchCapabilities
  , draw :: DrawCapabilities
  , gain :: GainCapabilities
  }

data SearchCapabilities = SearchCapabilities
  { deck :: InvestigatorMatcher
  }

data DrawCapabilities = DrawCapabilities
  { cards :: InvestigatorMatcher
  }

data GainCapabilities = GainCapabilities
  { resources :: InvestigatorMatcher
  }

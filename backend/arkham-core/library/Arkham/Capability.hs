{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Capability where

import Arkham.Matcher.Patterns
import Arkham.Matcher.Types
import Arkham.Modifier
import Arkham.Prelude

class Capable a where
  can :: Capabilities a

instance Capable InvestigatorMatcher where
  can =
    Capabilities
      { search = SearchCapabilities {deck = InvestigatorCanSearchDeck}
      , draw = DrawCapabilities {cards = InvestigatorWithoutModifier CannotDrawCards}
      , gain = GainCapabilities {resources = InvestigatorWithoutModifier CannotGainResources}
      }

data Capabilities a = Capabilities
  { search :: SearchCapabilities a
  , draw :: DrawCapabilities a
  , gain :: GainCapabilities a
  }
  deriving stock (Functor)

data SearchCapabilities a = SearchCapabilities
  { deck :: a
  }
  deriving stock (Functor)

data DrawCapabilities a = DrawCapabilities
  { cards :: a
  }
  deriving stock (Functor)

data GainCapabilities a = GainCapabilities
  { resources :: a
  }
  deriving stock (Functor)

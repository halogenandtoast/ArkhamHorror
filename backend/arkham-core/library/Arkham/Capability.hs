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
      , manipulate = ManipulateCapabilities {deck = InvestigatorWithoutModifier CannotManipulateDeck}
      , draw = DrawCapabilities {cards = InvestigatorWithoutModifier CannotDrawCards}
      , gain = GainCapabilities {resources = InvestigatorWithoutModifier CannotGainResources}
      , have =
          HaveCapabilities
            { cards =
                HaveCardsCapabilities {leaveDiscard = InvestigatorWithoutModifier CardsCannotLeaveYourDiscardPile}
            }
      }

data Capabilities a = Capabilities
  { search :: SearchCapabilities a
  , draw :: DrawCapabilities a
  , manipulate :: ManipulateCapabilities a
  , gain :: GainCapabilities a
  , have :: HaveCapabilities a
  }
  deriving stock (Functor)

data ManipulateCapabilities a = ManipulateCapabilities
  { deck :: a
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

data HaveCapabilities a = HaveCapabilities
  { cards :: HaveCardsCapabilities a
  }
  deriving stock (Functor)

data HaveCardsCapabilities a = HaveCardsCapabilities
  { leaveDiscard :: a
  }
  deriving stock (Functor)

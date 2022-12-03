module Arkham.Draw.Types where

import Arkham.Prelude

import Arkham.Card
import Arkham.Deck
import Arkham.Id

data CardDrawState = UnresolvedCardDraw | ResolvedCardDraw [Card]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CardDraw = CardDraw
  { cardDrawInvestigator :: InvestigatorId
  , cardDrawDeck :: DeckSignifier
  , cardDrawAmount :: Int
  , cardDrawState :: CardDrawState
  , cardDrawsesAction :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)


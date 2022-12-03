module Arkham.Draw.Types where

import Arkham.Prelude

import Arkham.Card
import Arkham.Deck
import Arkham.Id

data CardDrawState = UnresolvedCardDraw | ResolvedCardDraw [Card]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CardDraw = CardDraw
  { cardDrawsId :: CardDrawId
  , cardDrawsInvestigator :: InvestigatorId
  , cardDrawsDeck :: DeckSignifier
  , cardDrawsAmount :: Int
  , cardDrawsState :: CardDrawState
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)


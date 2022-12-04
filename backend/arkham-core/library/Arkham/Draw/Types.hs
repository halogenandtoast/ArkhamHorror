module Arkham.Draw.Types where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Deck
import Arkham.Id
import Arkham.Source

data CardDrawRules = ShuffleBackInEachWeakness
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data CardDrawState
  = UnresolvedCardDraw
  | InProgress [Card]
  | ResolvedCardDraw [Card]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CardDraw = CardDraw
  { cardDrawId :: CardDrawId
  , cardDrawInvestigator :: InvestigatorId
  , cardDrawSource :: Source
  , cardDrawDeck :: DeckSignifier
  , cardDrawAmount :: Int
  , cardDrawState :: CardDrawState
  , cardDrawAction :: Bool
  , cardDrawRules :: HashSet CardDrawRules
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

drewCard :: Card -> CardDraw -> CardDraw
drewCard c draw = draw { cardDrawState = updatedState }
 where
  updatedState = case cardDrawState draw of
    UnresolvedCardDraw -> InProgress [c]
    InProgress cs -> InProgress (c : cs)
    ResolvedCardDraw _ -> error "card draw was already finished"

newCardDraw
  :: (MonadRandom m, SourceEntity source)
  => InvestigatorId
  -> source
  -> Int
  -> m CardDraw
newCardDraw i source n = do
  drawId <- getRandom
  pure $ CardDraw
    { cardDrawId = drawId
    , cardDrawInvestigator = i
    , cardDrawSource = toSource source
    , cardDrawDeck = InvestigatorDeck i
    , cardDrawAmount = n
    , cardDrawState = UnresolvedCardDraw
    , cardDrawAction = False
    , cardDrawRules = mempty
    }

asDrawAction :: CardDraw -> CardDraw
asDrawAction c = c { cardDrawAction = True }

withCardDrawRule :: CardDrawRules -> CardDraw -> CardDraw
withCardDrawRule r c = c { cardDrawRules = insertSet r (cardDrawRules c) }

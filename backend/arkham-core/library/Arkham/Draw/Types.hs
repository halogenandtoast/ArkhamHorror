{-# LANGUAGE TemplateHaskell #-}

module Arkham.Draw.Types where

import Arkham.Prelude

import Arkham.Card
import Arkham.Deck
import Arkham.Id
import Arkham.Source
import Data.Aeson.TH
import GHC.Records

data CardDrawRules = ShuffleBackInEachWeakness | AfterDrawDiscard Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NoThunks)

data CardDrawState
  = UnresolvedCardDraw
  | InProgress [Card]
  | ResolvedCardDraw [Card]
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NoThunks)

data CardDraw = CardDraw
  { cardDrawId :: CardDrawId
  , cardDrawInvestigator :: InvestigatorId
  , cardDrawSource :: Source
  , cardDrawDeck :: DeckSignifier
  , cardDrawAmount :: Int
  , cardDrawState :: CardDrawState
  , cardDrawAction :: Bool
  , cardDrawRules :: Set CardDrawRules
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NoThunks)

instance HasField "investigator" CardDraw InvestigatorId where
  getField = cardDrawInvestigator

drewCard :: Card -> CardDraw -> CardDraw
drewCard c draw = draw {cardDrawState = updatedState}
 where
  updatedState = case cardDrawState draw of
    UnresolvedCardDraw -> InProgress [c]
    InProgress cs -> InProgress (c : cs)
    ResolvedCardDraw _ -> error "card draw was already finished"

newCardDraw
  :: (MonadRandom m, Sourceable source)
  => InvestigatorId
  -> source
  -> Int
  -> m CardDraw
newCardDraw i source n = do
  drawId <- getRandom
  pure
    $ CardDraw
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
asDrawAction c = c {cardDrawAction = True}

withCardDrawRule :: CardDrawRules -> CardDraw -> CardDraw
withCardDrawRule r c = c {cardDrawRules = insertSet r (cardDrawRules c)}

$(deriveJSON defaultOptions ''CardDrawRules)
$(deriveJSON defaultOptions ''CardDrawState)
$(deriveJSON defaultOptions ''CardDraw)

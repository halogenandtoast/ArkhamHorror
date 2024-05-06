{-# LANGUAGE TemplateHaskell #-}

module Arkham.Draw.Types where

import Arkham.Prelude

import Arkham.Card
import Arkham.Deck
import Arkham.Id
import Arkham.Scenario.Deck
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH
import GHC.Records

data CardDrawRules = ShuffleBackInEachWeakness | AfterDrawDiscard Int
  deriving stock (Show, Eq, Ord)

data CardDrawState
  = UnresolvedCardDraw
  | InProgress [Card]
  | ResolvedCardDraw [Card]
  deriving stock (Show, Eq, Ord)

data CardDrawKind = StandardCardDraw | StartingHandCardDraw
  deriving stock (Show, Eq, Ord)

data CardDraw msg = CardDraw
  { cardDrawSource :: Source
  , cardDrawDeck :: DeckSignifier
  , cardDrawAmount :: Int
  , cardDrawState :: CardDrawState
  , cardDrawTarget :: Maybe Target
  , cardDrawAction :: Bool
  , cardDrawKind :: CardDrawKind
  , cardDrawRules :: Set CardDrawRules
  , cardDrawAndThen :: Maybe msg
  , cardDrawHandler :: Maybe Target
  }
  deriving stock (Show, Eq, Ord)

instance HasField "kind" (CardDraw msg) CardDrawKind where
  getField = cardDrawKind

instance HasField "deck" (CardDraw msg) DeckSignifier where
  getField = cardDrawDeck

instance HasField "amount" (CardDraw msg) Int where
  getField = cardDrawAmount

data CardDrew = CardDrew
  { cardDrewSource :: Source
  , cardDrewDeck :: DeckSignifier
  , cardDrewCards :: [Card]
  , cardDrewAction :: Bool
  , cardDrewRules :: Set CardDrawRules
  , cardDrewTarget :: Maybe Target
  }
  deriving stock (Show, Eq, Ord)

instance HasField "isPlayerDraw" (CardDraw msg) Bool where
  getField _ = True

instance HasField "target" CardDrew (Maybe Target) where
  getField = cardDrewTarget

instance HasField "cards" CardDrew [Card] where
  getField = cardDrewCards

instance HasField "deck" CardDrew DeckSignifier where
  getField = cardDrewDeck

drewCard :: Card -> CardDraw msg -> CardDraw msg
drewCard c draw = draw {cardDrawState = updatedState}
 where
  updatedState = case cardDrawState draw of
    UnresolvedCardDraw -> InProgress [c]
    InProgress cs -> InProgress (c : cs)
    ResolvedCardDraw _ -> error "card draw was already finished"

class AsDeck a where
  asDeck :: a -> DeckSignifier

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
      , cardDrawHandler = Nothing
      }
>>>>>>> d02303435 (WIP)

instance AsDeck DeckSignifier where
  asDeck = id
  {-# INLINE asDeck #-}

instance AsDeck InvestigatorId where
  asDeck = InvestigatorDeck
  {-# INLINE asDeck #-}

instance AsDeck ScenarioDeckKey where
  asDeck = ScenarioDeckByKey

newCardDraw
  :: (Sourceable source, AsDeck deck)
  => source
  -> deck
  -> Int
  -> CardDraw msg
newCardDraw source deck n = do
  CardDraw
    { cardDrawSource = toSource source
    , cardDrawDeck = asDeck deck
    , cardDrawAmount = n
    , cardDrawState = UnresolvedCardDraw
    , cardDrawAction = False
    , cardDrawRules = mempty
    , cardDrawTarget = Nothing
    , cardDrawKind = StandardCardDraw
    , cardDrawAndThen = Nothing
    }

targetCardDraw
  :: (Targetable source, Sourceable source, AsDeck deck) => source -> deck -> Int -> CardDraw msg
targetCardDraw source deck n = setTarget source $ newCardDraw source deck n

finalizeDraw :: CardDraw msg -> [Card] -> CardDrew
finalizeDraw draw cards =
  CardDrew
    { cardDrewSource = cardDrawSource draw
    , cardDrewDeck = cardDrawDeck draw
    , cardDrewCards = cards
    , cardDrewAction = cardDrawAction draw
    , cardDrewRules = cardDrawRules draw
    , cardDrewTarget = cardDrawTarget draw
    }

instance WithTarget (CardDraw msg) where
  getTarget = cardDrawTarget
  setTarget t c = c {cardDrawTarget = Just (toTarget t)}

asDrawAction :: CardDraw msg -> CardDraw msg
asDrawAction c = c {cardDrawAction = True}

withCardDrawRule :: CardDrawRules -> CardDraw msg -> CardDraw msg
withCardDrawRule r c = c {cardDrawRules = insertSet r (cardDrawRules c)}

$(deriveJSON defaultOptions ''CardDrawKind)
$(deriveJSON defaultOptions ''CardDrawRules)
$(deriveJSON defaultOptions ''CardDrawState)
$(deriveJSON defaultOptions ''CardDraw)
$(deriveJSON defaultOptions ''CardDrew)

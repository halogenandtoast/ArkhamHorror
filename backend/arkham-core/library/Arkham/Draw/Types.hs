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

data CardDrawKind = StandardCardDraw | StartingHandCardDraw | RandomCardDraw
  deriving stock (Show, Eq, Ord)

data CardDraw = CardDraw
  { cardDrawSource :: Source
  , cardDrawDeck :: DeckSignifier
  , cardDrawAmount :: Int
  , cardDrawState :: CardDrawState
  , cardDrawTarget :: Maybe Target
  , cardDrawAction :: Bool
  , cardDrawKind :: CardDrawKind
  , cardDrawRules :: Set CardDrawRules
  }
  deriving stock (Show, Eq, Ord)

instance HasField "kind" CardDraw CardDrawKind where
  getField = cardDrawKind

data CardDrew = CardDrew
  { cardDrewSource :: Source
  , cardDrewDeck :: DeckSignifier
  , cardDrewCards :: [Card]
  , cardDrewAction :: Bool
  , cardDrewRules :: Set CardDrawRules
  , cardDrewTarget :: Maybe Target
  }
  deriving stock (Show, Eq, Ord)

instance HasField "isPlayerDraw" CardDraw Bool where
  getField _ = True

instance HasField "target" CardDrew (Maybe Target) where
  getField = cardDrewTarget

instance HasField "cards" CardDrew [Card] where
  getField = cardDrewCards

instance HasField "deck" CardDrew DeckSignifier where
  getField = cardDrewDeck

drewCard :: Card -> CardDraw -> CardDraw
drewCard c draw = draw {cardDrawState = updatedState}
 where
  updatedState = case cardDrawState draw of
    UnresolvedCardDraw -> InProgress [c]
    InProgress cs -> InProgress (c : cs)
    ResolvedCardDraw _ -> error "card draw was already finished"

class AsDeck a where
  asDeck :: a -> DeckSignifier

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
  -> CardDraw
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
    }

targetCardDraw
  :: (Targetable source, Sourceable source, AsDeck deck) => source -> deck -> Int -> CardDraw
targetCardDraw source deck n = setTarget source $ newCardDraw source deck n

randomTargetCardDraw
  :: (Targetable source, Sourceable source, AsDeck deck) => source -> deck -> Int -> CardDraw
randomTargetCardDraw source deck n = randomCardDraw $ setTarget source $ newCardDraw source deck n

instance WithTarget CardDraw where
  getTarget = cardDrawTarget
  setTarget t c = c {cardDrawTarget = Just (toTarget t)}

asDrawAction :: CardDraw -> CardDraw
asDrawAction c = c {cardDrawAction = True}

withCardDrawRule :: CardDrawRules -> CardDraw -> CardDraw
withCardDrawRule r c = c {cardDrawRules = insertSet r (cardDrawRules c)}

randomCardDraw :: CardDraw -> CardDraw
randomCardDraw c = c {cardDrawKind = RandomCardDraw}

$(deriveJSON defaultOptions ''CardDrawKind)
$(deriveJSON defaultOptions ''CardDrawRules)
$(deriveJSON defaultOptions ''CardDrawState)
$(deriveJSON defaultOptions ''CardDraw)
$(deriveJSON defaultOptions ''CardDrew)

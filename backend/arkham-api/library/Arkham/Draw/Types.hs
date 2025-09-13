{-# LANGUAGE TemplateHaskell #-}

module Arkham.Draw.Types where

import Arkham.Prelude

import Arkham.Card
import Arkham.Deck
import Arkham.Id
import Arkham.Matcher.Card (CardMatcher)
import Arkham.Scenario.Deck
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH
import GHC.Records

data CardDrawRules = ShuffleBackInEachWeakness | AfterDrawDiscard Int
  deriving stock (Show, Eq, Ord, Data)

data CardDrawState
  = UnresolvedCardDraw
  | InProgress [Card]
  | ResolvedCardDraw [Card]
  deriving stock (Show, Eq, Ord, Data)

data CardDrawKind = StandardCardDraw | StartingHandCardDraw
  deriving stock (Show, Eq, Ord, Data)

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
  , cardDrawAlreadyDrawn :: [Card]
  , cardDrawDiscard :: Maybe CardMatcher
  }
  deriving stock (Show, Eq, Ord, Data)

instance HasField "discard" (CardDraw msg) (Maybe CardMatcher) where
  getField = cardDrawDiscard

instance HasField "alreadyDrawn" (CardDraw msg) [Card] where
  getField = cardDrawAlreadyDrawn

instance HasField "kind" (CardDraw msg) CardDrawKind where
  getField = cardDrawKind

instance HasField "deck" (CardDraw msg) DeckSignifier where
  getField = cardDrawDeck

instance HasField "amount" (CardDraw msg) Int where
  getField = cardDrawAmount

setDrawDeck :: AsDeck deck => deck -> CardDraw msg -> CardDraw msg
setDrawDeck deck draw = draw {cardDrawDeck = asDeck deck}

data CardDrew = CardDrew
  { cardDrewSource :: Source
  , cardDrewDeck :: DeckSignifier
  , cardDrewCards :: [Card]
  , cardDrewAction :: Bool
  , cardDrewRules :: Set CardDrawRules
  , cardDrewTarget :: Maybe Target
  }
  deriving stock (Show, Eq, Ord, Data)

instance HasField "isEncounterDraw" (CardDraw msg) Bool where
  getField a = case a.deck of
    EncounterDeck {} -> True
    _ -> False

instance HasField "isPlayerDraw" (CardDraw msg) Bool where
  getField a = case a.deck of
    InvestigatorDeck {} -> True
    _ -> False

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
    , cardDrawAlreadyDrawn = []
    , cardDrawDiscard = Nothing
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

shuffleBackInEachWeakness :: CardDraw msg -> CardDraw msg
shuffleBackInEachWeakness = withCardDrawRule ShuffleBackInEachWeakness

$(deriveJSON defaultOptions ''CardDrawKind)
$(deriveJSON defaultOptions ''CardDrawRules)
$(deriveJSON defaultOptions ''CardDrawState)
$(deriveToJSON defaultOptions ''CardDraw)

instance FromJSON msg => FromJSON (CardDraw msg) where
  parseJSON = withObject "CardDraw" \o -> do
    cardDrawSource <- o .: "cardDrawSource"
    cardDrawDeck <- o .: "cardDrawDeck"
    cardDrawAmount <- o .: "cardDrawAmount"
    cardDrawState <- o .: "cardDrawState"
    cardDrawTarget <- o .: "cardDrawTarget"
    cardDrawAction <- o .: "cardDrawAction"
    cardDrawKind <- o .: "cardDrawKind"
    cardDrawRules <- o .: "cardDrawRules"
    cardDrawAndThen <- o .: "cardDrawAndThen"
    cardDrawAlreadyDrawn <- o .:? "cardDrawAlreadyDrawn" .!= []
    cardDrawDiscard <- o .:? "cardDrawDiscard"
    pure CardDraw {..}

$(deriveJSON defaultOptions ''CardDrew)

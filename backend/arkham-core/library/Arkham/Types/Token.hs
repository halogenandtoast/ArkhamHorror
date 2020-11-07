module Arkham.Types.Token where

import ClassyPrelude
import Data.Aeson
import Data.UUID

newtype TokenId = TokenId { getTokenId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, Hashable)

data TokenModifier = PositiveModifier Int | NegativeModifier Int | DoubleNegativeModifier Int | AutoFailModifier | NoModifier
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Monoid TokenModifier where
  mempty = PositiveModifier 0

-- TODO: this is a huge bandaid and might not work later
instance Semigroup TokenModifier where
  AutoFailModifier <> _ = AutoFailModifier
  _ <> AutoFailModifier = AutoFailModifier
  NoModifier <> a = a
  a <> NoModifier = a
  a <> b =
    let
      calc =
        fromMaybe 0 (tokenModifierToInt a) + fromMaybe 0 (tokenModifierToInt b)
    in
      case compare 0 calc of
        EQ -> PositiveModifier calc
        GT -> PositiveModifier calc
        LT -> NegativeModifier calc

data TokenValue = TokenValue Token TokenModifier
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

tokenValue :: TokenValue -> Maybe Int
tokenValue (TokenValue _ modifier) = tokenModifierToInt modifier

tokenModifierToInt :: TokenModifier -> Maybe Int
tokenModifierToInt = \case
  PositiveModifier n -> Just n
  NegativeModifier n -> Just (-n)
  DoubleNegativeModifier n -> Just (-(n * 2))
  AutoFailModifier -> Nothing
  NoModifier -> Just 0

tokenModifier :: TokenValue -> TokenModifier
tokenModifier (TokenValue _ x) = x

data DrawnToken = DrawnToken
  { drawnTokenId :: TokenId
  , drawnTokenFace :: Token
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data Token
  = PlusOne
  | Zero
  | MinusOne
  | MinusTwo
  | MinusThree
  | MinusFour
  | MinusFive
  | MinusSix
  | MinusSeven
  | MinusEight
  | Skull
  | Cultist
  | Tablet
  | ElderThing
  | AutoFail
  | ElderSign
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

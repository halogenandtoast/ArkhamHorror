module Arkham.Types.Token where

import Arkham.Prelude

newtype TokenId = TokenId { getTokenId :: UUID }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, Random)

data TokenModifier = PositiveModifier Int | NegativeModifier Int | ZeroModifier | DoubleNegativeModifier Int | AutoFailModifier | NoModifier
  deriving stock (Ord, Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Monoid TokenModifier where
  mempty = ZeroModifier

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

data TokenValue = TokenValue TokenFace TokenModifier
  deriving stock (Ord, Show, Eq, Generic)
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
  ZeroModifier -> Just 0

data Token = Token
  { tokenId :: TokenId
  , tokenFace :: TokenFace
  }
  deriving stock (Ord, Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TokenFace
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
  deriving stock (Ord, Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

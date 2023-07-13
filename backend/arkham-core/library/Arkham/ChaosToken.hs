{-# LANGUAGE TemplateHaskell #-}

module Arkham.ChaosToken where

import Arkham.Prelude

import Data.Aeson.TH

newtype ChaosTokenId = ChaosTokenId {getChaosTokenId :: UUID}
  deriving newtype (Show, Eq, ToJSON, FromJSON, Ord, Random)

data ChaosTokenModifier
  = PositiveModifier Int
  | NegativeModifier Int
  | ZeroModifier
  | DoubleNegativeModifier Int
  | AutoFailModifier
  | AutoSuccessModifier
  | NoModifier
  deriving stock (Show, Eq, Ord)

instance Monoid ChaosTokenModifier where
  mempty = NoModifier

-- TODO: this is a huge bandaid and might not work later
instance Semigroup ChaosTokenModifier where
  -- If a skill test both automatically succeeds and automatically fails, [for
  -- instance by drawing {Autofail} and {ElderSign} with Olive McBride while
  -- playing as Father Mateo], the automatic failure takes precedence, and the
  -- test automatically fails.
  AutoFailModifier <> _ = AutoFailModifier
  _ <> AutoFailModifier = AutoFailModifier
  AutoSuccessModifier <> _ = AutoSuccessModifier
  _ <> AutoSuccessModifier = AutoSuccessModifier
  NoModifier <> a = a
  a <> NoModifier = a
  a <> b =
    let
      calc =
        fromMaybe 0 (chaosTokenModifierToInt a) + fromMaybe 0 (chaosTokenModifierToInt b)
    in
      case compare 0 calc of
        EQ -> PositiveModifier calc
        GT -> PositiveModifier calc
        LT -> NegativeModifier calc

data ChaosTokenValue = ChaosTokenValue ChaosTokenFace ChaosTokenModifier
  deriving stock (Show, Eq)

chaosTokenValue :: ChaosTokenValue -> Maybe Int
chaosTokenValue (ChaosTokenValue _ modifier) = chaosTokenModifierToInt modifier

chaosTokenModifierToInt :: ChaosTokenModifier -> Maybe Int
chaosTokenModifierToInt = \case
  PositiveModifier n -> Just n
  NegativeModifier n -> Just (-n)
  DoubleNegativeModifier n -> Just (-(n * 2))
  AutoSuccessModifier -> Nothing
  AutoFailModifier -> Nothing
  NoModifier -> Just 0
  ZeroModifier -> Just 0

data ChaosToken = ChaosToken
  { chaosTokenId :: ChaosTokenId
  , chaosTokenFace :: ChaosTokenFace
  }
  deriving stock (Show, Eq, Ord)

data ChaosTokenFace
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
  deriving stock (Bounded, Enum, Show, Eq, Ord)

isNumberChaosToken :: ChaosTokenFace -> Bool
isNumberChaosToken = \case
  PlusOne -> True
  Zero -> True
  MinusOne -> True
  MinusTwo -> True
  MinusThree -> True
  MinusFour -> True
  MinusFive -> True
  MinusSix -> True
  MinusSeven -> True
  MinusEight -> True
  Skull -> False
  Cultist -> False
  Tablet -> False
  ElderThing -> False
  AutoFail -> False
  ElderSign -> False

isEvenChaosToken :: ChaosTokenFace -> Bool
isEvenChaosToken = \case
  PlusOne -> False
  Zero -> True
  MinusOne -> False
  MinusTwo -> True
  MinusThree -> False
  MinusFour -> True
  MinusFive -> False
  MinusSix -> True
  MinusSeven -> False
  MinusEight -> True
  Skull -> False
  Cultist -> False
  Tablet -> False
  ElderThing -> False
  AutoFail -> False
  ElderSign -> False

isOddChaosToken :: ChaosTokenFace -> Bool
isOddChaosToken = \case
  PlusOne -> True
  Zero -> False
  MinusOne -> True
  MinusTwo -> False
  MinusThree -> True
  MinusFour -> False
  MinusFive -> True
  MinusSix -> False
  MinusSeven -> True
  MinusEight -> False
  Skull -> False
  Cultist -> False
  Tablet -> False
  ElderThing -> False
  AutoFail -> False
  ElderSign -> False

isSymbolChaosToken :: ChaosTokenFace -> Bool
isSymbolChaosToken = \case
  PlusOne -> False
  Zero -> False
  MinusOne -> False
  MinusTwo -> False
  MinusThree -> False
  MinusFour -> False
  MinusFive -> False
  MinusSix -> False
  MinusSeven -> False
  MinusEight -> False
  Skull -> True
  Cultist -> True
  Tablet -> True
  ElderThing -> True
  AutoFail -> True
  ElderSign -> True

$(deriveJSON defaultOptions ''ChaosTokenModifier)
$(deriveJSON defaultOptions ''ChaosTokenFace)
$(deriveJSON defaultOptions ''ChaosToken)
$(deriveJSON defaultOptions ''ChaosTokenValue)

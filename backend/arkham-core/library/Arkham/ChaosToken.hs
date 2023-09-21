{-# LANGUAGE TemplateHaskell #-}

module Arkham.ChaosToken where

import Arkham.Prelude

import Data.Aeson.TH
import GHC.OverloadedLabels

newtype ChaosTokenId = ChaosTokenId {getChaosTokenId :: UUID}
  deriving stock (Data)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Ord, Random)

data ChaosTokenModifier
  = PositiveModifier Int
  | NegativeModifier Int
  | ZeroModifier
  | DoubleNegativeModifier Int
  | AutoFailModifier
  | AutoSuccessModifier
  | NoModifier
  deriving stock (Show, Eq, Ord, Data)

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
  deriving stock (Show, Eq, Data)

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
  deriving stock (Show, Eq, Ord, Data)

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
  deriving stock (Bounded, Enum, Show, Eq, Ord, Data)

instance IsLabel "skull" ChaosTokenFace where
  fromLabel = Skull

instance IsLabel "cultist" ChaosTokenFace where
  fromLabel = Cultist

instance IsLabel "tablet" ChaosTokenFace where
  fromLabel = Tablet

instance IsLabel "elderthing" ChaosTokenFace where
  fromLabel = ElderThing

instance IsLabel "autofail" ChaosTokenFace where
  fromLabel = AutoFail

instance IsLabel "eldersign" ChaosTokenFace where
  fromLabel = ElderSign

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

isNonNegativeChaosToken :: ChaosTokenFace -> Bool
isNonNegativeChaosToken = \case
  PlusOne -> True
  Zero -> True
  MinusOne -> False
  MinusTwo -> False
  MinusThree -> False
  MinusFour -> False
  MinusFive -> False
  MinusSix -> False
  MinusSeven -> False
  MinusEight -> False
  Skull -> False
  Cultist -> False
  Tablet -> False
  ElderThing -> False
  AutoFail -> False
  ElderSign -> False

-- only for the printed value and should not be used in scenario
-- mainly used for the JudgementXX Tarot Card
chaosTokenToFaceValue :: ChaosTokenFace -> Int
chaosTokenToFaceValue = \case
  PlusOne -> 1
  Zero -> 0
  MinusOne -> (-1)
  MinusTwo -> (-2)
  MinusThree -> (-3)
  MinusFour -> (-4)
  MinusFive -> (-5)
  MinusSix -> (-6)
  MinusSeven -> (-7)
  MinusEight -> (-8)
  Skull -> 0
  Cultist -> 0
  Tablet -> 0
  ElderThing -> 0
  AutoFail -> 0
  ElderSign -> 0

$(deriveJSON defaultOptions ''ChaosTokenModifier)
$(deriveJSON defaultOptions ''ChaosTokenFace)
$(deriveJSON defaultOptions ''ChaosToken)
$(deriveJSON defaultOptions ''ChaosTokenValue)

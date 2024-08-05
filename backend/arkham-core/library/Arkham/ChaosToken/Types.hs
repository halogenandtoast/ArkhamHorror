{-# LANGUAGE TemplateHaskell #-}

module Arkham.ChaosToken.Types where

import {-# SOURCE #-} Arkham.Calculation
import Arkham.Classes.GameLogger
import Arkham.Id
import Arkham.Prelude
import Data.Aeson.TH
import GHC.OverloadedLabels
import GHC.Records

newtype ChaosTokenId = ChaosTokenId {getChaosTokenId :: UUID}
  deriving stock Data
  deriving newtype (Show, Eq, ToJSON, FromJSON, Ord, Random)

data ChaosTokenModifier
  = AutoFailModifier
  | AutoSuccessModifier
  | CalculatedModifier GameCalculation
  | NoModifier
  deriving stock (Show, Eq, Ord, Data)

data ChaosTokenValue = ChaosTokenValue ChaosTokenFace ChaosTokenModifier
  deriving stock (Show, Eq, Ord, Data)

data ChaosToken = ChaosToken
  { chaosTokenId :: ChaosTokenId
  , chaosTokenFace :: ChaosTokenFace
  , chaosTokenRevealedBy :: Maybe InvestigatorId
  }
  deriving stock (Show, Data)

instance Ord ChaosToken where
  compare a b = compare (chaosTokenId a) (chaosTokenId b)

instance Eq ChaosToken where
  a == b = chaosTokenId a == chaosTokenId b

instance ToGameLoggerFormat ChaosToken where
  format c = format c.face

instance ToGameLoggerFormat ChaosTokenFace where
  format c = "{token:\"" <> tshow c <> "\"}"

instance HasField "id" ChaosToken ChaosTokenId where
  getField = chaosTokenId

instance HasField "face" ChaosToken ChaosTokenFace where
  getField = chaosTokenFace

instance HasField "revealedBy" ChaosToken (Maybe InvestigatorId) where
  getField = chaosTokenRevealedBy

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
  | CurseToken
  | BlessToken
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

instance IsLabel "bless" ChaosTokenFace where
  fromLabel = BlessToken

instance IsLabel "curse" ChaosTokenFace where
  fromLabel = CurseToken

allChaosTokenFaces :: [ChaosTokenFace]
allChaosTokenFaces = [minBound ..]

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
  CurseToken -> False
  BlessToken -> False

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
  CurseToken -> False
  BlessToken -> False

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
  CurseToken -> False
  BlessToken -> False

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
  CurseToken -> True
  BlessToken -> True

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
  BlessToken -> False
  CurseToken -> False

isNegativeChaosToken :: ChaosTokenFace -> Bool
isNegativeChaosToken = not . isNonNegativeChaosToken

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
  CurseToken -> 0
  BlessToken -> 0

chaosTokenLabel :: ChaosTokenFace -> Text
chaosTokenLabel = \case
  PlusOne -> "+1"
  Zero -> "0"
  MinusOne -> "-1"
  MinusTwo -> "-2"
  MinusThree -> "-3"
  MinusFour -> "-4"
  MinusFive -> "-5"
  MinusSix -> "-6"
  MinusSeven -> "-7"
  MinusEight -> "-8"
  Skull -> "Skull"
  Cultist -> "Cultist"
  Tablet -> "Tablet"
  ElderThing -> "Elder Thing"
  AutoFail -> "Auto Fail"
  ElderSign -> "Elder Sign"
  CurseToken -> "Curse"
  BlessToken -> "Bless"

$(deriveJSON defaultOptions ''ChaosTokenModifier)
$(deriveJSON defaultOptions ''ChaosTokenFace)
$(deriveJSON defaultOptions ''ChaosToken)
$(deriveJSON defaultOptions ''ChaosTokenValue)

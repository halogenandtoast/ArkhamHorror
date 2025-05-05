{-# LANGUAGE TemplateHaskell #-}

module Arkham.ChaosToken.Types where

import {-# SOURCE #-} Arkham.Calculation
import Arkham.Classes.GameLogger
import Arkham.Id
import Arkham.Prelude
import Data.Aeson.TH
import Data.Function (on)
import GHC.OverloadedLabels
import GHC.Records

newtype ChaosTokenId = ChaosTokenId UUID
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

createChaosToken :: MonadRandom m => ChaosTokenFace -> m ChaosToken
createChaosToken face = do
  tokenId <- getRandom
  pure $ ChaosToken tokenId face Nothing False False

data ChaosToken = ChaosToken
  { chaosTokenId :: ChaosTokenId
  , chaosTokenFace :: ChaosTokenFace
  , chaosTokenRevealedBy :: Maybe InvestigatorId
  , chaosTokenCancelled :: Bool
  , chaosTokenSealed :: Bool
  }
  deriving stock (Show, Data)

revealedByL :: Lens' ChaosToken (Maybe InvestigatorId)
revealedByL = lens chaosTokenRevealedBy \m x -> m {chaosTokenRevealedBy = x}

instance Ord ChaosToken where
  compare = compare `on` chaosTokenId

instance Eq ChaosToken where
  (==) = (==) `on` chaosTokenId

instance ToGameLoggerFormat ChaosToken where
  format c = format c.face

instance ToGameLoggerFormat ChaosTokenFace where
  format c = "{token:\"" <> tshow c <> "\"}"

instance ToDisplay ChaosTokenFace where
  toDisplay = \case
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
    Skull -> "{skull}"
    Cultist -> "{cultist}"
    Tablet -> "{tablet}"
    ElderThing -> "{elderThing}"
    AutoFail -> "{autoFail}"
    ElderSign -> "{elderSign}"
    CurseToken -> "{curse}"
    BlessToken -> "{bless}"
    FrostToken -> "{frost}"

instance HasField "id" ChaosToken ChaosTokenId where
  getField = chaosTokenId

instance HasField "face" ChaosToken ChaosTokenFace where
  getField = chaosTokenFace

instance HasField "sealed" ChaosToken Bool where
  getField = chaosTokenSealed

instance HasField "revealedBy" ChaosToken (Maybe InvestigatorId) where
  getField = chaosTokenRevealedBy

instance HasField "cancelled" ChaosToken Bool where
  getField = chaosTokenCancelled

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
  | FrostToken
  deriving stock (Bounded, Enum, Show, Eq, Ord, Data)

instance IsLabel "+1" ChaosTokenFace where
  fromLabel = PlusOne

instance IsLabel "0" ChaosTokenFace where
  fromLabel = Zero

instance IsLabel "-1" ChaosTokenFace where
  fromLabel = MinusOne

instance IsLabel "-2" ChaosTokenFace where
  fromLabel = MinusTwo

instance IsLabel "-3" ChaosTokenFace where
  fromLabel = MinusThree

instance IsLabel "-4" ChaosTokenFace where
  fromLabel = MinusFour

instance IsLabel "-5" ChaosTokenFace where
  fromLabel = MinusFive

instance IsLabel "-6" ChaosTokenFace where
  fromLabel = MinusSix

instance IsLabel "-7" ChaosTokenFace where
  fromLabel = MinusSeven

instance IsLabel "-8" ChaosTokenFace where
  fromLabel = MinusEight

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

instance IsLabel "frost" ChaosTokenFace where
  fromLabel = FrostToken

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
  _ -> False

isEvenChaosToken :: ChaosTokenFace -> Bool
isEvenChaosToken = \case
  Zero -> True
  MinusTwo -> True
  MinusFour -> True
  MinusSix -> True
  MinusEight -> True
  _ -> False

isOddChaosToken :: ChaosTokenFace -> Bool
isOddChaosToken = \case
  PlusOne -> True
  MinusOne -> True
  MinusThree -> True
  MinusFive -> True
  MinusSeven -> True
  _ -> False

isSymbolChaosToken :: ChaosTokenFace -> Bool
isSymbolChaosToken = \case
  Skull -> True
  Cultist -> True
  Tablet -> True
  ElderThing -> True
  AutoFail -> True
  ElderSign -> True
  CurseToken -> True
  BlessToken -> True
  FrostToken -> True
  _ -> False

isNonNegativeChaosToken :: ChaosTokenFace -> Bool
isNonNegativeChaosToken = \case
  PlusOne -> True
  Zero -> True
  _ -> False

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
  _ -> 0

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
  FrostToken -> "Frost"

mconcat
  [ deriveJSON defaultOptions ''ChaosTokenModifier
  , deriveJSON defaultOptions ''ChaosTokenFace
  , deriveToJSON defaultOptions ''ChaosToken
  , [d|
      instance FromJSON ChaosToken where
        parseJSON = withObject "ChaosToken" \o -> do
          chaosTokenId <- o .: "chaosTokenId"
          chaosTokenFace <- o .: "chaosTokenFace"
          chaosTokenRevealedBy <- o .: "chaosTokenRevealedBy"
          chaosTokenCancelled <- o .:? "chaosTokenCancelled" .!= False
          chaosTokenSealed <- o .:? "chaosTokenSealed" .!= False
          pure ChaosToken {..}
      |]
  , deriveJSON defaultOptions ''ChaosTokenValue
  ]

instance ToJSONKey ChaosTokenFace
instance FromJSONKey ChaosTokenFace

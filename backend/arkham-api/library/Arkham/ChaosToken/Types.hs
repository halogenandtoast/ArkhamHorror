{-# LANGUAGE TemplateHaskell #-}

module Arkham.ChaosToken.Types where

import {-# SOURCE #-} Arkham.Calculation
import Arkham.Classes.GameLogger
import Arkham.Id
import Arkham.Prelude
import Data.Aeson.TH
import Data.Char qualified as Char
import Data.Function (on)
import Data.Text qualified as T
import GHC.OverloadedLabels
import GHC.Records


-- | The display key of a custom token: the part after the final colon
-- (":circus-ex-mortis:moon" -> "moon"). Drives the format tag ("{moon}"),
-- the label ("Moon"), and the frontend icon key (ct_moon.png).
customTokenKey :: Text -> Text
customTokenKey slug = case T.splitOn ":" slug of
  [] -> slug
  parts -> last (impureNonNull parts)

capitalizeFirst :: Text -> Text
capitalizeFirst t = case T.uncons t of
  Nothing -> t
  Just (c, rest) -> T.cons (Char.toUpper c) rest

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
    CustomToken slug -> "{" <> customTokenKey slug <> "}"

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
  | -- | Open constructor for campaign/scenario-specific homebrew tokens,
    -- identified by slug (e.g. @":circus-ex-mortis:moon"@). Display name,
    -- icon key, and JSON form derive from the slug's last segment; reveal
    -- behavior comes from 'Arkham.Homebrew.Tokens.customTokenDefs'.
    CustomToken Text
  deriving stock (Show, Eq, Ord, Data)

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

-- | All official token faces (custom homebrew faces are open-ended and
-- deliberately excluded).
allChaosTokenFaces :: [ChaosTokenFace]
allChaosTokenFaces =
  [ PlusOne, Zero, MinusOne, MinusTwo, MinusThree, MinusFour, MinusFive
  , MinusSix, MinusSeven, MinusEight, Skull, Cultist, Tablet, ElderThing
  , AutoFail, ElderSign, CurseToken, BlessToken, FrostToken
  ]

instance HasField "isNumber" ChaosTokenFace Bool where
  getField = isNumberChaosToken

instance HasField "isSymbol" ChaosTokenFace Bool where
  getField = isSymbolChaosToken

instance HasField "isEven" ChaosTokenFace Bool where
  getField = isEvenChaosToken

instance HasField "isOdd" ChaosTokenFace Bool where
  getField = isOddChaosToken

instance HasField "isNonNegative" ChaosTokenFace Bool where
  getField = isNonNegativeChaosToken

instance HasField "isNegative" ChaosTokenFace Bool where
  getField = isNegativeChaosToken

instance HasField "faceValue" ChaosTokenFace Int where
  getField = chaosTokenToFaceValue

instance HasField "label" ChaosTokenFace Text where
  getField = chaosTokenLabel

{- FOURMOLU_DISABLE -}
instance HasField "isStandard" ChaosTokenFace Bool where
  getField = (`elem` standardTokens)
    where
      standardTokens =
        [ PlusOne, Zero, MinusOne, MinusTwo, MinusThree, MinusFour
        , MinusFive, MinusSix, MinusSeven, MinusEight
        , Skull, Cultist, Tablet, ElderThing, AutoFail, ElderSign
        ]
{- FOURMOLU_ENABLE -}

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
  CustomToken _ -> True
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
  CustomToken slug -> capitalizeFirst (customTokenKey slug)

mconcat
  [ deriveJSON defaultOptions ''ChaosTokenModifier
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

-- | Kept compatible with the original all-nullary string encoding: official
-- faces encode as their constructor name, custom faces as their slug. Unknown
-- strings parse as 'CustomToken' (with the legacy "MoonToken" name remapped),
-- so saves survive both directions.
instance ToJSON ChaosTokenFace where
  toJSON (CustomToken slug) = String slug
  toJSON face = String (tshow face)

officialChaosTokenFaces :: Map Text ChaosTokenFace
officialChaosTokenFaces = mapFromList [(tshow face, face) | face <- allChaosTokenFaces]

instance FromJSON ChaosTokenFace where
  parseJSON = withText "ChaosTokenFace" \t ->
    pure $ fromMaybe (CustomToken (legacyTokenSlug t)) (lookup t officialChaosTokenFaces)

legacyTokenSlug :: Text -> Text
legacyTokenSlug "MoonToken" = ":circus-ex-mortis:moon"
legacyTokenSlug t = t

{-# LANGUAGE TemplateHaskell #-}

module Arkham.Key where

import Arkham.ChaosToken.Types
import Arkham.Prelude
import Data.Aeson.TH
import Data.UUID (fromWords64)

data ArkhamKey
  = TokenKey ChaosToken
  | RedKey
  | BlueKey
  | GreenKey
  | YellowKey
  | PurpleKey
  | BlackKey
  | WhiteKey
  | UnrevealedKey ArkhamKey
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, ToJSONKey)

class IsKey a where
  toKey :: a -> ArkhamKey

instance IsKey ChaosToken where
  toKey = TokenKey

instance IsKey ArkhamKey where
  toKey = id

keyName :: ArkhamKey -> Text
keyName = \case
  TokenKey token -> chaosTokenLabel token.face
  RedKey -> "Red"
  BlueKey -> "Blue"
  GreenKey -> "Green"
  YellowKey -> "Yellow"
  PurpleKey -> "Purple"
  BlackKey -> "Black"
  WhiteKey -> "White"
  UnrevealedKey _ -> "Unrevealed"

mconcat
  [ makePrisms ''ArkhamKey
  , [d|
      instance FromJSON ArkhamKey where
        parseJSON o = flip (withObject "ArkhamKey") o \v -> do
          tag :: Text <- v .: "tag"
          case tag of
            "SkullKey" ->
              pure
                $ TokenKey
                $ ChaosToken
                  { chaosTokenId = coerce $ fromWords64 6128981282234515924 12039885860129472512
                  , chaosTokenFace = #skull
                  , chaosTokenRevealedBy = Nothing
                  , chaosTokenCancelled = False
                  , chaosTokenSealed = False
                  }
            "CultistKey" ->
              pure
                $ TokenKey
                $ ChaosToken
                  { chaosTokenId = coerce $ fromWords64 9237114041627526267 13667317789959208286
                  , chaosTokenFace = #cultist
                  , chaosTokenRevealedBy = Nothing
                  , chaosTokenCancelled = False
                  , chaosTokenSealed = False
                  }
            "TabletKey" ->
              pure
                $ TokenKey
                $ ChaosToken
                  { chaosTokenId = coerce $ fromWords64 17367115229369288362 9986755224281741872
                  , chaosTokenFace = #tablet
                  , chaosTokenRevealedBy = Nothing
                  , chaosTokenCancelled = False
                  , chaosTokenSealed = False
                  }
            "ElderThingKey" ->
              pure
                $ TokenKey
                $ ChaosToken
                  { chaosTokenId = coerce $ fromWords64 17260509442849393188 12601461981560257361
                  , chaosTokenFace = #elderthing
                  , chaosTokenRevealedBy = Nothing
                  , chaosTokenCancelled = False
                  , chaosTokenSealed = False
                  }
            _ -> $(mkParseJSON defaultOptions ''ArkhamKey) o
      |]
  ]

instance FromJSONKey ArkhamKey

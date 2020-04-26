module Arkham.Types.Location where

import Data.Text
import GHC.Generics
import Json
import Prelude (Int, Show)

data ArkhamClueCount = ArkhamClueCountNumber Int | ArkhamClueCountPerInvestigator Int
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via TaggedJson "clueCount" ArkhamClueCount

data ArkhamLocationSymbol = ArkhamLocationSymbolCircle | ArkhamLocationSymbolSquare | ArkhamLocationSymbolHeart
  deriving stock (Show, Generic)
  deriving (ToJSON, FromJSON) via TaggedJson "symbol" ArkhamLocationSymbol

data ArkhamLocationUnrevealedData = ArkhamLocationUnrevealedData
  { arkhamLocationUnrevealedDataId :: Text
  , arkhamLocationUnrevealedDataName :: Text
  , arkhamLocationUnrevealedDataSymbol :: ArkhamLocationSymbol
  , arkhamLocationUnrevealedDataImageUrl :: Text
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamLocationUnrevealedData") ArkhamLocationUnrevealedData

data ArkhamLocationRevealedData = ArkhamLocationRevealedData
  { arkhamLocationRevealedDataId :: Text
  , arkhamLocationRevealedDataName :: Text
  , arkhamLocationRevealedDataSymbol :: ArkhamLocationSymbol
  , arkhamLocationRevealedDataConnections :: [ArkhamLocationSymbol]
  , arkhamLocationRevealedDataShroud :: Int
  , arkhamLocationRevealedDataMaxClues :: ArkhamClueCount
  , arkhamLocationRevealedDataCurrentClues :: Int
  , arkhamLocationRevealedDataImageUrl :: Text
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamLocationRevealedData") ArkhamLocationRevealedData

data ArkhamLocation = ArkhamLocationUnrevealed ArkhamLocationUnrevealedData | ArkhamLocationRevealed ArkhamLocationRevealedData
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via TaggedJson "location" ArkhamLocation


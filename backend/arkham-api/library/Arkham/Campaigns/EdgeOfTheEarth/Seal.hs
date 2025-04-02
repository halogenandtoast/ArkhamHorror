module Arkham.Campaigns.EdgeOfTheEarth.Seal where

import Arkham.Prelude
import Arkham.Id
import Data.Function (on)
import GHC.Records

data Seal = Seal
  { sealKind :: SealKind
  , sealActive :: Bool
  , sealPlacedBy :: Maybe InvestigatorId
  }
  deriving stock (Show, Ord, Generic, Data)
  deriving anyclass (ToJSON)

instance FromJSON Seal where
  parseJSON = withObject "Seal" \o -> do
    sealKind <- o .: "sealKind"
    sealActive <- o .: "sealActive"
    sealPlacedBy <- o .:? "sealPlacedBy"
    pure Seal {..}

instance HasField "kind" Seal SealKind where
  getField = sealKind

instance HasField "active" Seal Bool where
  getField = sealActive

data SealKind = SealA | SealB | SealC | SealD | SealE
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance Eq Seal where
  (==) = (==) `on` sealKind

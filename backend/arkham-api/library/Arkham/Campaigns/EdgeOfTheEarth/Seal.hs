module Arkham.Campaigns.EdgeOfTheEarth.Seal where

import Arkham.Prelude
import Data.Function (on)
import GHC.Records

data Seal = Seal
  { sealKind :: SealKind
  , sealActive :: Bool
  }
  deriving stock (Show, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance HasField "kind" Seal SealKind where
  getField = sealKind

instance HasField "active" Seal Bool where
  getField = sealActive

data SealKind = SealA | SealB | SealC | SealD | SealE
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance Eq Seal where
  (==) = (==) `on` sealKind

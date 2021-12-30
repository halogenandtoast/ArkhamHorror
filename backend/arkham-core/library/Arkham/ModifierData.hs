module Arkham.ModifierData
  ( module Arkham.ModifierData
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Id
import Arkham.Modifier

newtype ModifierData = ModifierData { mdModifiers :: [Modifier] }
  deriving stock (Show, Eq, Generic)

instance ToJSON ModifierData where
  toJSON = genericToJSON $ aesonOptions $ Just "md"
  toEncoding = genericToEncoding $ aesonOptions $ Just "md"

newtype ConnectionData = ConnectionData { cdConnectedLocations :: [LocationId] }
  deriving stock (Show, Eq, Generic)

instance ToJSON ConnectionData where
  toJSON = genericToJSON $ aesonOptions $ Just "cd"
  toEncoding = genericToEncoding $ aesonOptions $ Just "cd"

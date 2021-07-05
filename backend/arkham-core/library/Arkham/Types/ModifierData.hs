module Arkham.Types.ModifierData
  ( module Arkham.Types.ModifierData
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Modifier

newtype ModifierData = ModifierData { mdModifiers :: [Modifier] }
  deriving stock (Show, Eq, Generic)

instance ToJSON ModifierData where
  toJSON = genericToJSON $ aesonOptions $ Just "md"
  toEncoding = genericToEncoding $ aesonOptions $ Just "md"

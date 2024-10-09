module Arkham.Modifier where

import Arkham.Prelude

data Modifier
data ModifierType

instance Ord ModifierType
instance Data ModifierType
instance Show ModifierType
instance Eq ModifierType
instance ToJSON ModifierType
instance FromJSON ModifierType

module Arkham.Entities where

import Arkham.Prelude
import Arkham.Classes.Entity

data Entities

instance Eq Entities
instance Show Entities
type EntityMap a = HashMap (EntityId a) a

module Arkham.Entities where

import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Id
import Arkham.Prelude

data Entities

instance Monoid Entities
instance Eq Entities
instance Show Entities
instance HasAbilities Entities
type EntityMap a = Map (EntityId a) a

addCardEntityWith
  :: InvestigatorId -> (forall a. (Typeable a) => a -> a) -> Entities -> Card -> Entities

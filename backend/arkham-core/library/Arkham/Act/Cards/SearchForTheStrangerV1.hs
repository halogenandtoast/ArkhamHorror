module Arkham.Act.Cards.SearchForTheStrangerV1
  ( SearchForTheStrangerV1(..)
  , searchForTheStrangerV1
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype SearchForTheStrangerV1 = SearchForTheStrangerV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheStrangerV1 :: ActCard SearchForTheStrangerV1
searchForTheStrangerV1 = act (2, A) SearchForTheStrangerV1 Cards.searchForTheStrangerV1 Nothing

instance RunMessage SearchForTheStrangerV1 where
  runMessage msg (SearchForTheStrangerV1 attrs) = SearchForTheStrangerV1 <$> runMessage msg attrs

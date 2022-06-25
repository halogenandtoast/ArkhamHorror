module Arkham.Act.Cards.SearchForTheStrangerV3
  ( SearchForTheStrangerV3(..)
  , searchForTheStrangerV3
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype SearchForTheStrangerV3 = SearchForTheStrangerV3 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheStrangerV3 :: ActCard SearchForTheStrangerV3
searchForTheStrangerV3 = act (2, A) SearchForTheStrangerV3 Cards.searchForTheStrangerV3 Nothing

instance RunMessage SearchForTheStrangerV3 where
  runMessage msg (SearchForTheStrangerV3 attrs) = SearchForTheStrangerV3 <$> runMessage msg attrs

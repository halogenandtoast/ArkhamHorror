module Arkham.Act.Cards.SearchForTheStrangerV2
  ( SearchForTheStrangerV2(..)
  , searchForTheStrangerV2
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype SearchForTheStrangerV2 = SearchForTheStrangerV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheStrangerV2 :: ActCard SearchForTheStrangerV2
searchForTheStrangerV2 = act (2, A) SearchForTheStrangerV2 Cards.searchForTheStrangerV2 Nothing

instance RunMessage SearchForTheStrangerV2 where
  runMessage msg (SearchForTheStrangerV2 attrs) = SearchForTheStrangerV2 <$> runMessage msg attrs

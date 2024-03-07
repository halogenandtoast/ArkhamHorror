module Arkham.Act.Cards.SearchingTheUnnamable
  ( SearchingTheUnnamable(..)
  , searchingTheUnnamable
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype SearchingTheUnnamable = SearchingTheUnnamable ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

searchingTheUnnamable :: ActCard SearchingTheUnnamable
searchingTheUnnamable = act (1, A) SearchingTheUnnamable Cards.searchingTheUnnamable Nothing

instance RunMessage SearchingTheUnnamable where
  runMessage msg (SearchingTheUnnamable attrs) = SearchingTheUnnamable <$> runMessage msg attrs

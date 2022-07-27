module Arkham.Act.Cards.SearchForTheRuins
  ( SearchForTheRuins(..)
  , searchForTheRuins
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype SearchForTheRuins = SearchForTheRuins ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheRuins :: ActCard SearchForTheRuins
searchForTheRuins = act (3, A) SearchForTheRuins Cards.searchForTheRuins Nothing

instance RunMessage SearchForTheRuins where
  runMessage msg (SearchForTheRuins attrs) = SearchForTheRuins <$> runMessage msg attrs

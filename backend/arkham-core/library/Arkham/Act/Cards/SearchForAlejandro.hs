module Arkham.Act.Cards.SearchForAlejandro
  ( SearchForAlejandro(..)
  , searchForAlejandro
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype SearchForAlejandro = SearchForAlejandro ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

searchForAlejandro :: ActCard SearchForAlejandro
searchForAlejandro =
  act (1, C) SearchForAlejandro Cards.searchForAlejandro Nothing

instance RunMessage SearchForAlejandro where
  runMessage msg (SearchForAlejandro attrs) =
    SearchForAlejandro <$> runMessage msg attrs

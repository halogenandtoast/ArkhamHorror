module Arkham.Act.Cards.SearchForTheBrotherhood
  ( SearchForTheBrotherhood(..)
  , searchForTheBrotherhood
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype SearchForTheBrotherhood = SearchForTheBrotherhood ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

searchForTheBrotherhood :: ActCard SearchForTheBrotherhood
searchForTheBrotherhood =
  act (2, A) SearchForTheBrotherhood Cards.searchForTheBrotherhood Nothing

instance RunMessage SearchForTheBrotherhood where
  runMessage msg (SearchForTheBrotherhood attrs) =
    SearchForTheBrotherhood <$> runMessage msg attrs

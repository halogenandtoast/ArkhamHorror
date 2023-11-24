module Arkham.Act.Cards.SearchForThePatient
  ( SearchForThePatient(..)
  , searchForThePatient
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype SearchForThePatient = SearchForThePatient ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

searchForThePatient :: ActCard SearchForThePatient
searchForThePatient = act (2, A) SearchForThePatient Cards.searchForThePatient Nothing

instance RunMessage SearchForThePatient where
  runMessage msg (SearchForThePatient attrs) = SearchForThePatient <$> runMessage msg attrs

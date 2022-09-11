module Arkham.Act.Cards.MissingPersons
  ( MissingPersons(..)
  , missingPersons
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype MissingPersons = MissingPersons ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

missingPersons :: ActCard MissingPersons
missingPersons = act (1, C) MissingPersons Cards.missingPersons Nothing

instance RunMessage MissingPersons where
  runMessage msg (MissingPersons attrs) =
    MissingPersons <$> runMessage msg attrs

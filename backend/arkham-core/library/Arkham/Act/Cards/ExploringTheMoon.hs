module Arkham.Act.Cards.ExploringTheMoon
  ( ExploringTheMoon(..)
  , exploringTheMoon
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype ExploringTheMoon = ExploringTheMoon ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

exploringTheMoon :: ActCard ExploringTheMoon
exploringTheMoon = act (2, A) ExploringTheMoon Cards.exploringTheMoon Nothing

instance RunMessage ExploringTheMoon where
  runMessage msg (ExploringTheMoon attrs) = ExploringTheMoon <$> runMessage msg attrs

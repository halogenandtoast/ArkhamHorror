module Arkham.Act.Cards.ExploringPnakotus
  ( ExploringPnakotus(..)
  , exploringPnakotus
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype ExploringPnakotus = ExploringPnakotus ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

exploringPnakotus :: ActCard ExploringPnakotus
exploringPnakotus =
  act (1, A) ExploringPnakotus Cards.exploringPnakotus Nothing

instance RunMessage ExploringPnakotus where
  runMessage msg (ExploringPnakotus attrs) =
    ExploringPnakotus <$> runMessage msg attrs

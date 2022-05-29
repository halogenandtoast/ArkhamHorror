module Arkham.Act.Cards.ThroughTheCatacombs
  ( ThroughTheCatacombs(..)
  , throughTheCatacombs
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Attrs
import Arkham.Act.Runner
import Arkham.Classes

newtype ThroughTheCatacombs = ThroughTheCatacombs ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

throughTheCatacombs :: ActCard ThroughTheCatacombs
throughTheCatacombs = act (1, A) ThroughTheCatacombs Cards.throughTheCatacombs Nothing

instance ActRunner env => RunMessage env ThroughTheCatacombs where
  runMessage msg (ThroughTheCatacombs attrs) = ThroughTheCatacombs <$> runMessage msg attrs

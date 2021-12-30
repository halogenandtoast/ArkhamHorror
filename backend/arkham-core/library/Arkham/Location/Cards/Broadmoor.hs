module Arkham.Location.Cards.Broadmoor
  ( Broadmoor(..)
  , broadmoor
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (broadmoor)
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Attrs

newtype Broadmoor = Broadmoor LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

broadmoor :: LocationCard Broadmoor
broadmoor =
  location Broadmoor Cards.broadmoor 3 (PerPlayer 1) Plus [Square, Plus]

instance HasAbilities Broadmoor where
  getAbilities (Broadmoor a) = withResignAction a []

instance LocationRunner env => RunMessage env Broadmoor where
  runMessage msg (Broadmoor attrs) = Broadmoor <$> runMessage msg attrs

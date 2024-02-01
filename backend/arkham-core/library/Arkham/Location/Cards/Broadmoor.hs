module Arkham.Location.Cards.Broadmoor (
  Broadmoor (..),
  broadmoor,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (broadmoor)
import Arkham.Location.Runner

newtype Broadmoor = Broadmoor LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

broadmoor :: LocationCard Broadmoor
broadmoor = location Broadmoor Cards.broadmoor 3 (PerPlayer 1)

instance HasAbilities Broadmoor where
  getAbilities (Broadmoor a) = withResignAction a []

instance RunMessage Broadmoor where
  runMessage msg (Broadmoor attrs) = Broadmoor <$> runMessage msg attrs

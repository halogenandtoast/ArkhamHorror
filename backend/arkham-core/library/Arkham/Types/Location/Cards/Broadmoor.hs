module Arkham.Types.Location.Cards.Broadmoor
  ( Broadmoor(..)
  , broadmoor
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (broadmoor)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype Broadmoor = Broadmoor LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

broadmoor :: LocationCard Broadmoor
broadmoor =
  location Broadmoor Cards.broadmoor 3 (PerPlayer 1) Plus [Square, Plus]

instance HasAbilities env Broadmoor where
  getAbilities _ _ (Broadmoor a) = pure [locationResignAction a]

instance LocationRunner env => RunMessage env Broadmoor where
  runMessage msg (Broadmoor attrs) = Broadmoor <$> runMessage msg attrs

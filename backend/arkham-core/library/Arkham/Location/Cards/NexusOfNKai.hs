module Arkham.Location.Cards.NexusOfNKai
  ( nexusOfNKai
  , NexusOfNKai(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype NexusOfNKai = NexusOfNKai LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nexusOfNKai :: LocationCard NexusOfNKai
nexusOfNKai = location NexusOfNKai Cards.nexusOfNKai 4 (PerPlayer 1)

instance HasAbilities NexusOfNKai where
  getAbilities (NexusOfNKai attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage NexusOfNKai where
  runMessage msg (NexusOfNKai attrs) = NexusOfNKai <$> runMessage msg attrs

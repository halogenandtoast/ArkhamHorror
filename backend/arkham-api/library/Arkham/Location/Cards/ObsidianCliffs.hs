module Arkham.Location.Cards.ObsidianCliffs (obsidianCliffs) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ObsidianCliffs = ObsidianCliffs LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obsidianCliffs :: LocationCard ObsidianCliffs
obsidianCliffs = location ObsidianCliffs Cards.obsidianCliffs 4 (Static 1)

-- TODO: abilities

instance RunMessage ObsidianCliffs where
  runMessage msg (ObsidianCliffs attrs) = runQueueT $ ObsidianCliffs <$> liftRunMessage msg attrs

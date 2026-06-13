module Arkham.Location.Cards.ObsidianFoundations (obsidianFoundations) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ObsidianFoundations = ObsidianFoundations LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obsidianFoundations :: LocationCard ObsidianFoundations
obsidianFoundations = location ObsidianFoundations Cards.obsidianFoundations 0 (Static 2)

-- TODO: abilities

instance RunMessage ObsidianFoundations where
  runMessage msg (ObsidianFoundations attrs) = runQueueT $ ObsidianFoundations <$> liftRunMessage msg attrs

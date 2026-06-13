module Arkham.Location.Cards.OtherworldlyMechanismsObsidianBulwark (otherworldlyMechanismsObsidianBulwark) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype OtherworldlyMechanismsObsidianBulwark = OtherworldlyMechanismsObsidianBulwark LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherworldlyMechanismsObsidianBulwark :: LocationCard OtherworldlyMechanismsObsidianBulwark
otherworldlyMechanismsObsidianBulwark = location OtherworldlyMechanismsObsidianBulwark Cards.otherworldlyMechanismsObsidianBulwark 2 (Static 2)

-- TODO: abilities

instance RunMessage OtherworldlyMechanismsObsidianBulwark where
  runMessage msg (OtherworldlyMechanismsObsidianBulwark attrs) = runQueueT $ OtherworldlyMechanismsObsidianBulwark <$> liftRunMessage msg attrs

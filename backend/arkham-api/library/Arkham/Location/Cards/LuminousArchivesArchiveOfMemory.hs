module Arkham.Location.Cards.LuminousArchivesArchiveOfMemory (luminousArchivesArchiveOfMemory) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LuminousArchivesArchiveOfMemory = LuminousArchivesArchiveOfMemory LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luminousArchivesArchiveOfMemory :: LocationCard LuminousArchivesArchiveOfMemory
luminousArchivesArchiveOfMemory = location LuminousArchivesArchiveOfMemory Cards.luminousArchivesArchiveOfMemory 2 (Static 3)

-- TODO: abilities

instance RunMessage LuminousArchivesArchiveOfMemory where
  runMessage msg (LuminousArchivesArchiveOfMemory attrs) = runQueueT $ LuminousArchivesArchiveOfMemory <$> liftRunMessage msg attrs

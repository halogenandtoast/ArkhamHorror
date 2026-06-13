module Arkham.Location.Cards.LuminousArchivesArchiveOfHistory (luminousArchivesArchiveOfHistory) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LuminousArchivesArchiveOfHistory = LuminousArchivesArchiveOfHistory LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luminousArchivesArchiveOfHistory :: LocationCard LuminousArchivesArchiveOfHistory
luminousArchivesArchiveOfHistory = location LuminousArchivesArchiveOfHistory Cards.luminousArchivesArchiveOfHistory 3 (Static 2)

-- TODO: abilities

instance RunMessage LuminousArchivesArchiveOfHistory where
  runMessage msg (LuminousArchivesArchiveOfHistory attrs) = runQueueT $ LuminousArchivesArchiveOfHistory <$> liftRunMessage msg attrs

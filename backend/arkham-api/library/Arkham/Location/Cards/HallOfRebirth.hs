module Arkham.Location.Cards.HallOfRebirth
  ( hallOfRebirth
  , HallOfRebirth(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype HallOfRebirth = HallOfRebirth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallOfRebirth :: LocationCard HallOfRebirth
hallOfRebirth = location HallOfRebirth Cards.hallOfRebirth 2 (Static 1)

instance HasAbilities HallOfRebirth where
  getAbilities (HallOfRebirth attrs) =
    extendRevealed attrs []

instance RunMessage HallOfRebirth where
  runMessage msg (HallOfRebirth attrs) = runQueueT $ case msg of
    _ -> HallOfRebirth <$> liftRunMessage msg attrs

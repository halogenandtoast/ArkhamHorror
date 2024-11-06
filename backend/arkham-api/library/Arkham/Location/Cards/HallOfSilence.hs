module Arkham.Location.Cards.HallOfSilence
  ( hallOfSilence
  , HallOfSilence(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype HallOfSilence = HallOfSilence LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallOfSilence :: LocationCard HallOfSilence
hallOfSilence = location HallOfSilence Cards.hallOfSilence 4 (PerPlayer 1)

instance HasAbilities HallOfSilence where
  getAbilities (HallOfSilence attrs) =
    extendRevealed attrs []

instance RunMessage HallOfSilence where
  runMessage msg (HallOfSilence attrs) = runQueueT $ case msg of
    _ -> HallOfSilence <$> liftRunMessage msg attrs

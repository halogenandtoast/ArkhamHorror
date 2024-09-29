module Arkham.Location.Cards.HiddenCove
  ( hiddenCove
  , HiddenCove(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype HiddenCove = HiddenCove LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenCove :: LocationCard HiddenCove
hiddenCove = location HiddenCove Cards.hiddenCove 3 (Static 0)

instance HasAbilities HiddenCove where
  getAbilities (HiddenCove attrs) =
    extendRevealed attrs []

instance RunMessage HiddenCove where
  runMessage msg (HiddenCove attrs) = runQueueT $ case msg of
    _ -> HiddenCove <$> liftRunMessage msg attrs

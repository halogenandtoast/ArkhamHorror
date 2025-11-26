module Arkham.Location.Cards.Anchorage (anchorage) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype Anchorage = Anchorage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anchorage :: LocationCard Anchorage
anchorage = location Anchorage Cards.anchorage 0 (Static 0)

instance HasAbilities Anchorage where
  getAbilities (Anchorage attrs) =
    extendRevealed attrs []

instance RunMessage Anchorage where
  runMessage msg (Anchorage attrs) = runQueueT $ case msg of
    _ -> Anchorage <$> liftRunMessage msg attrs

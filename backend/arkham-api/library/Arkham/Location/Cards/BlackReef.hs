module Arkham.Location.Cards.BlackReef (
  blackReef,
  BlackReef (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted

newtype BlackReef = BlackReef LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackReef :: LocationCard BlackReef
blackReef = locationWith BlackReef Cards.blackReef 2 (Static 0) connectsToAdjacent

instance HasAbilities BlackReef where
  getAbilities (BlackReef attrs) =
    extendRevealed attrs []

instance RunMessage BlackReef where
  runMessage msg (BlackReef attrs) = runQueueT $ case msg of
    _ -> BlackReef <$> liftRunMessage msg attrs

module Arkham.Location.Cards.TheBourseLocusSafeguard (theBourseLocusSafeguard) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheBourseLocusSafeguard = TheBourseLocusSafeguard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBourseLocusSafeguard :: LocationCard TheBourseLocusSafeguard
theBourseLocusSafeguard = symbolLabel $ location TheBourseLocusSafeguard Cards.theBourseLocusSafeguard 0 (Static 0)

instance HasAbilities TheBourseLocusSafeguard where
  getAbilities (TheBourseLocusSafeguard a) =
    extendRevealed a []

instance RunMessage TheBourseLocusSafeguard where
  runMessage msg (TheBourseLocusSafeguard attrs) = runQueueT $ case msg of
    _ -> TheBourseLocusSafeguard <$> liftRunMessage msg attrs

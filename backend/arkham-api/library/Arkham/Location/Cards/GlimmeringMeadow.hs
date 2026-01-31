module Arkham.Location.Cards.GlimmeringMeadow (glimmeringMeadow) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GlimmeringMeadow = GlimmeringMeadow LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glimmeringMeadow :: LocationCard GlimmeringMeadow
glimmeringMeadow = locationWith GlimmeringMeadow Cards.glimmeringMeadow 2 (Static 0) connectsToAdjacent

instance HasAbilities GlimmeringMeadow where
  getAbilities (GlimmeringMeadow a) =
    extendRevealed a []

instance RunMessage GlimmeringMeadow where
  runMessage msg (GlimmeringMeadow attrs) = runQueueT $ case msg of
    _ -> GlimmeringMeadow <$> liftRunMessage msg attrs

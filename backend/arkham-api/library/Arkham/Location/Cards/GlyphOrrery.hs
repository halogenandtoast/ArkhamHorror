module Arkham.Location.Cards.GlyphOrrery (glyphOrrery) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GlyphOrrery = GlyphOrrery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glyphOrrery :: LocationCard GlyphOrrery
glyphOrrery = location GlyphOrrery Cards.glyphOrrery 4 (Static 1)

-- TODO: abilities

instance RunMessage GlyphOrrery where
  runMessage msg (GlyphOrrery attrs) = runQueueT $ GlyphOrrery <$> liftRunMessage msg attrs

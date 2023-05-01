module Arkham.Location.Cards.ChapelAtticSpectral_176
  ( chapelAtticSpectral_176
  , ChapelAtticSpectral_176(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype ChapelAtticSpectral_176 = ChapelAtticSpectral_176 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapelAtticSpectral_176 :: LocationCard ChapelAtticSpectral_176
chapelAtticSpectral_176 = location ChapelAtticSpectral_176 Cards.chapelAtticSpectral_176 8 (Static 0)

instance HasAbilities ChapelAtticSpectral_176 where
  getAbilities (ChapelAtticSpectral_176 attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage ChapelAtticSpectral_176 where
  runMessage msg (ChapelAtticSpectral_176 attrs) =
    ChapelAtticSpectral_176 <$> runMessage msg attrs

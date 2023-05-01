module Arkham.Location.Cards.ChapelAtticSpectral_175
  ( chapelAtticSpectral_175
  , ChapelAtticSpectral_175(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype ChapelAtticSpectral_175 = ChapelAtticSpectral_175 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapelAtticSpectral_175 :: LocationCard ChapelAtticSpectral_175
chapelAtticSpectral_175 = location ChapelAtticSpectral_175 Cards.chapelAtticSpectral_175 4 (Static 0)

instance HasAbilities ChapelAtticSpectral_175 where
  getAbilities (ChapelAtticSpectral_175 attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage ChapelAtticSpectral_175 where
  runMessage msg (ChapelAtticSpectral_175 attrs) =
    ChapelAtticSpectral_175 <$> runMessage msg attrs

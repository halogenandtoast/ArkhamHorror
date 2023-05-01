module Arkham.Location.Cards.ChapelCryptSpectral_174
  ( chapelCryptSpectral_174
  , ChapelCryptSpectral_174(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype ChapelCryptSpectral_174 = ChapelCryptSpectral_174 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapelCryptSpectral_174 :: LocationCard ChapelCryptSpectral_174
chapelCryptSpectral_174 = location ChapelCryptSpectral_174 Cards.chapelCryptSpectral_174 0 (Static 0)

instance HasAbilities ChapelCryptSpectral_174 where
  getAbilities (ChapelCryptSpectral_174 attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage ChapelCryptSpectral_174 where
  runMessage msg (ChapelCryptSpectral_174 attrs) =
    ChapelCryptSpectral_174 <$> runMessage msg attrs

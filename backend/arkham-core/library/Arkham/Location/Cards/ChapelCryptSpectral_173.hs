module Arkham.Location.Cards.ChapelCryptSpectral_173
  ( chapelCryptSpectral_173
  , ChapelCryptSpectral_173(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype ChapelCryptSpectral_173 = ChapelCryptSpectral_173 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapelCryptSpectral_173 :: LocationCard ChapelCryptSpectral_173
chapelCryptSpectral_173 = location ChapelCryptSpectral_173 Cards.chapelCryptSpectral_173 6 (Static 0)

instance HasAbilities ChapelCryptSpectral_173 where
  getAbilities (ChapelCryptSpectral_173 attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage ChapelCryptSpectral_173 where
  runMessage msg (ChapelCryptSpectral_173 attrs) =
    ChapelCryptSpectral_173 <$> runMessage msg attrs

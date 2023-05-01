module Arkham.Location.Cards.ChapelCrypt_173
  ( chapelCrypt_173
  , ChapelCrypt_173(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype ChapelCrypt_173 = ChapelCrypt_173 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapelCrypt_173 :: LocationCard ChapelCrypt_173
chapelCrypt_173 = location ChapelCrypt_173 Cards.chapelCrypt_173 6 (Static 0)

instance HasAbilities ChapelCrypt_173 where
  getAbilities (ChapelCrypt_173 attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage ChapelCrypt_173 where
  runMessage msg (ChapelCrypt_173 attrs) =
    ChapelCrypt_173 <$> runMessage msg attrs

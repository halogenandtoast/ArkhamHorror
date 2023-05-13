module Arkham.Location.Cards.ChapelCrypt_173 (
  chapelCrypt_173,
  ChapelCrypt_173 (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
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
  runMessage msg l@(ChapelCrypt_173 attrs) = case msg of
    Flip _ _ target | isTarget attrs target -> do
      spectral <- genCard Locations.chapelCryptSpectral_173
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    _ -> ChapelCrypt_173 <$> runMessage msg attrs

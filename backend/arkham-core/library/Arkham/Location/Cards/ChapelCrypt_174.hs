module Arkham.Location.Cards.ChapelCrypt_174 (
  chapelCrypt_174,
  ChapelCrypt_174 (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner

newtype ChapelCrypt_174 = ChapelCrypt_174 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapelCrypt_174 :: LocationCard ChapelCrypt_174
chapelCrypt_174 = location ChapelCrypt_174 Cards.chapelCrypt_174 6 (Static 0)

instance HasAbilities ChapelCrypt_174 where
  getAbilities (ChapelCrypt_174 attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage ChapelCrypt_174 where
  runMessage msg l@(ChapelCrypt_174 attrs) = case msg of
    Flip _ _ target | isTarget attrs target -> do
      spectral <- genCard Locations.chapelCryptSpectral_174
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    _ -> ChapelCrypt_174 <$> runMessage msg attrs

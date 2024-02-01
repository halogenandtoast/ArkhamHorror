module Arkham.Location.Cards.ChapelCrypt_174 (
  chapelCrypt_174,
  ChapelCrypt_174 (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait (Trait (Hex))

newtype ChapelCrypt_174 = ChapelCrypt_174 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

chapelCrypt_174 :: LocationCard ChapelCrypt_174
chapelCrypt_174 = location ChapelCrypt_174 Cards.chapelCrypt_174 6 (Static 0)

instance HasModifiersFor ChapelCrypt_174 where
  getModifiersFor target (ChapelCrypt_174 attrs) | isTarget attrs target = do
    shouldModifyShroud <-
      selectNone $ investigatorAt (toId attrs) <> HasMatchingTreachery (TreacheryWithTrait Hex)
    pure $ toModifiers attrs [ShroudModifier (-3) | shouldModifyShroud]
  getModifiersFor _ _ = pure []

instance RunMessage ChapelCrypt_174 where
  runMessage msg l@(ChapelCrypt_174 attrs) = case msg of
    Flip _ _ target | isTarget attrs target -> do
      spectral <- genCard Locations.chapelCryptSpectral_174
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    _ -> ChapelCrypt_174 <$> runMessage msg attrs

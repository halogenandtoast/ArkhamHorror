module Arkham.Location.Cards.ChapelCrypt_174 (chapelCrypt_174, ChapelCrypt_174 (..)) where

import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))
import Arkham.Trait (Trait (Hex))

newtype ChapelCrypt_174 = ChapelCrypt_174 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

chapelCrypt_174 :: LocationCard ChapelCrypt_174
chapelCrypt_174 = location ChapelCrypt_174 Cards.chapelCrypt_174 6 (Static 0)

instance HasModifiersFor ChapelCrypt_174 where
  getModifiersFor (ChapelCrypt_174 a) = maybeModifySelf a do
    liftGuardM $ selectNone $ investigatorAt a <> HasMatchingTreachery (TreacheryWithTrait Hex)
    pure [ShroudModifier (-3)]

instance RunMessage ChapelCrypt_174 where
  runMessage msg l@(ChapelCrypt_174 attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      spectral <- genCard Locations.chapelCryptSpectral_174
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    _ -> ChapelCrypt_174 <$> liftRunMessage msg attrs

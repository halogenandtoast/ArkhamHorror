module Arkham.Location.Cards.ChapelCrypt_173 (chapelCrypt_173, ChapelCrypt_173 (..)) where

import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))

newtype ChapelCrypt_173 = ChapelCrypt_173 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

chapelCrypt_173 :: LocationCard ChapelCrypt_173
chapelCrypt_173 = location ChapelCrypt_173 Cards.chapelCrypt_173 6 (Static 0)

instance HasModifiersFor ChapelCrypt_173 where
  getModifiersFor (ChapelCrypt_173 a) = maybeModifySelf a do
    liftGuardM $ selectNone $ enemyAt a <> ReadyEnemy
    pure [ShroudModifier (-3)]

instance RunMessage ChapelCrypt_173 where
  runMessage msg l@(ChapelCrypt_173 attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      spectral <- genCard Locations.chapelCryptSpectral_173
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    _ -> ChapelCrypt_173 <$> liftRunMessage msg attrs

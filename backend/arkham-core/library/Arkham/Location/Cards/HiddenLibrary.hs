module Arkham.Location.Cards.HiddenLibrary (
  hiddenLibrary,
  HiddenLibrary (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait

newtype HiddenLibrary = HiddenLibrary LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

hiddenLibrary :: LocationCard HiddenLibrary
hiddenLibrary = location HiddenLibrary Cards.hiddenLibrary 4 (PerPlayer 3)

instance HasModifiersFor HiddenLibrary where
  getModifiersFor (LocationTarget lid) (HiddenLibrary attrs)
    | toId attrs == lid = do
        enemyIsMoving <- isJust <$> selectOne MovingEnemy
        pure $ toModifiers attrs [AddTrait Passageway | enemyIsMoving]
  getModifiersFor _ _ = pure []

instance RunMessage HiddenLibrary where
  runMessage msg (HiddenLibrary attrs) = HiddenLibrary <$> runMessage msg attrs

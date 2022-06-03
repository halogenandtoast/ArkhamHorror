module Arkham.Location.Cards.HiddenLibrary
  ( hiddenLibrary
  , HiddenLibrary(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Target
import Arkham.Trait

newtype HiddenLibrary = HiddenLibrary LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hiddenLibrary :: LocationCard HiddenLibrary
hiddenLibrary =
  location HiddenLibrary Cards.hiddenLibrary 4 (PerPlayer 3) NoSymbol []

instance Query EnemyMatcher env => HasModifiersFor env HiddenLibrary where
  getModifiersFor _ (LocationTarget lid) (HiddenLibrary attrs)
    | toId attrs == lid = do
      enemyIsMoving <- isJust <$> selectOne MovingEnemy
      pure $ toModifiers attrs [ AddTrait Passageway | enemyIsMoving ]
  getModifiersFor _ _ _ = pure []

instance LocationRunner env => RunMessage HiddenLibrary where
  runMessage msg (HiddenLibrary attrs) = HiddenLibrary <$> runMessage msg attrs

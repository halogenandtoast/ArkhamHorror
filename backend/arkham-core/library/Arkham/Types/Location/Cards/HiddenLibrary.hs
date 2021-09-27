module Arkham.Types.Location.Cards.HiddenLibrary
  ( hiddenLibrary
  , HiddenLibrary(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Trait

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

instance LocationRunner env => RunMessage env HiddenLibrary where
  runMessage msg (HiddenLibrary attrs) = HiddenLibrary <$> runMessage msg attrs

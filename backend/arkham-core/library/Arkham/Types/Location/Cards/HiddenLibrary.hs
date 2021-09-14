module Arkham.Types.Location.Cards.HiddenLibrary
  ( hiddenLibrary
  , HiddenLibrary(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait

newtype HiddenLibrary = HiddenLibrary LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hiddenLibrary :: LocationCard HiddenLibrary
hiddenLibrary =
  location HiddenLibrary Cards.hiddenLibrary 4 (PerPlayer 3) NoSymbol []

instance HasModifiersFor env HiddenLibrary where
  getModifiersFor (EnemySource _) (LocationTarget lid) (HiddenLibrary attrs)
    | toId attrs == lid = pure $ toModifiers attrs [AddTrait Passageway]
  getModifiersFor _ _ _ = pure []

instance LocationRunner env => RunMessage env HiddenLibrary where
  runMessage msg (HiddenLibrary attrs) = HiddenLibrary <$> runMessage msg attrs

module Arkham.Location.Cards.HiddenLibrary (hiddenLibrary, HiddenLibrary (..)) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait

newtype HiddenLibrary = HiddenLibrary LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hiddenLibrary :: LocationCard HiddenLibrary
hiddenLibrary = location HiddenLibrary Cards.hiddenLibrary 4 (PerPlayer 3)

instance HasModifiersFor HiddenLibrary where
  getModifiersFor (HiddenLibrary a) = maybeModifySelf a do
    liftGuardM $ selectAny MovingEnemy
    pure [AddTrait Passageway]

instance RunMessage HiddenLibrary where
  runMessage msg (HiddenLibrary attrs) = HiddenLibrary <$> runMessage msg attrs

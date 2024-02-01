module Arkham.Location.Cards.OrneLibrary where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (orneLibrary)
import Arkham.Location.Helpers
import Arkham.Location.Runner

newtype OrneLibrary = OrneLibrary LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

orneLibrary :: LocationCard OrneLibrary
orneLibrary = location OrneLibrary Cards.orneLibrary 3 (PerPlayer 1)

instance HasModifiersFor OrneLibrary where
  getModifiersFor (InvestigatorTarget iid) (OrneLibrary attrs) = do
    here <- iid `isAt` attrs
    pure $ toModifiers attrs [ActionCostOf #investigate 1 | here]
  getModifiersFor _ _ = pure []

instance RunMessage OrneLibrary where
  runMessage msg (OrneLibrary attrs) = OrneLibrary <$> runMessage msg attrs

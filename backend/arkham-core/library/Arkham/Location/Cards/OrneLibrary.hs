module Arkham.Location.Cards.OrneLibrary where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (orneLibrary)
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Modifier
import Arkham.Target

newtype OrneLibrary = OrneLibrary LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

orneLibrary :: LocationCard OrneLibrary
orneLibrary =
  location OrneLibrary Cards.orneLibrary 3 (PerPlayer 1) Triangle [Plus, Square]

instance HasModifiersFor OrneLibrary where
  getModifiersFor _ (InvestigatorTarget iid) (OrneLibrary attrs)
    | iid `elem` locationInvestigators attrs = pure
    $ toModifiers attrs [ActionCostOf (IsAction Action.Investigate) 1]
  getModifiersFor _ _ _ = pure []

instance LocationRunner env => RunMessage OrneLibrary where
  runMessage msg (OrneLibrary attrs) = OrneLibrary <$> runMessage msg attrs

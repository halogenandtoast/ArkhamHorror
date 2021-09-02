module Arkham.Types.Location.Cards.OrneLibrary where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (orneLibrary)
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype OrneLibrary = OrneLibrary LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

orneLibrary :: LocationCard OrneLibrary
orneLibrary =
  location OrneLibrary Cards.orneLibrary 3 (PerPlayer 1) Triangle [Plus, Square]

instance HasModifiersFor env OrneLibrary where
  getModifiersFor _ (InvestigatorTarget iid) (OrneLibrary attrs)
    | iid `elem` locationInvestigators attrs = pure
    $ toModifiers attrs [ActionCostOf (IsAction Action.Investigate) 1]
  getModifiersFor _ _ _ = pure []

instance LocationRunner env => RunMessage env OrneLibrary where
  runMessage msg (OrneLibrary attrs) = OrneLibrary <$> runMessage msg attrs

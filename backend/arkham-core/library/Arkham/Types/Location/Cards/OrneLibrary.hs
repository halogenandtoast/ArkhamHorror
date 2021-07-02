module Arkham.Types.Location.Cards.OrneLibrary where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (orneLibrary)
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype OrneLibrary = OrneLibrary LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

orneLibrary :: LocationId -> OrneLibrary
orneLibrary = OrneLibrary . baseAttrs
  Cards.orneLibrary
  3
  (PerPlayer 1)
  Triangle
  [Plus, Square]

instance HasModifiersFor env OrneLibrary where
  getModifiersFor _ target (OrneLibrary attrs) | isTarget attrs target =
    pure $ toModifiers attrs [ActionCostOf (IsAction Action.Investigate) 1]
  getModifiersFor _ (InvestigatorTarget iid) (OrneLibrary attrs)
    | iid `elem` locationInvestigators attrs = pure
    $ toModifiers attrs [ActionCostOf (IsAction Action.Investigate) 1]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env OrneLibrary where
  getActions i window (OrneLibrary attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env OrneLibrary where
  runMessage msg (OrneLibrary attrs) = OrneLibrary <$> runMessage msg attrs

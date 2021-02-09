module Arkham.Types.Location.Cards.OrneLibrary where


import qualified Arkham.Types.Action as Action
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype OrneLibrary = OrneLibrary LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

orneLibrary :: OrneLibrary
orneLibrary = OrneLibrary $ base { locationVictory = Just 1 }
 where
  base = baseAttrs
    "02050"
    (Name "Orne Library" Nothing)
    EncounterSet.ExtracurricularActivity
    3
    (PerPlayer 1)
    Triangle
    [Plus, Square]
    [Miskatonic]

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

module Arkham.Types.Location.Cards.RitualGrounds where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (ritualGrounds)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message

newtype RitualGrounds = RitualGrounds LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritualGrounds :: LocationCard RitualGrounds
ritualGrounds = location
  RitualGrounds
  Cards.ritualGrounds
  2
  (PerPlayer 1)
  Equals
  [Hourglass, Equals]

instance HasModifiersFor env RitualGrounds

instance HasAbilities env RitualGrounds where
  getAbilities i window (RitualGrounds attrs) = getAbilities i window attrs

instance (LocationRunner env) => RunMessage env RitualGrounds where
  runMessage msg l@(RitualGrounds attrs@LocationAttrs {..}) = case msg of
    EndTurn iid | iid `elem` locationInvestigators -> l <$ pushAll
      [ DrawCards iid 1 False
      , InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
      ]
    _ -> RitualGrounds <$> runMessage msg attrs

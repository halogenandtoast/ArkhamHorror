module Arkham.Types.Location.Cards.TheEdgeOfTheUniverse
  ( theEdgeOfTheUniverse
  , TheEdgeOfTheUniverse(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Name
import Arkham.Types.Trait

newtype TheEdgeOfTheUniverse = TheEdgeOfTheUniverse LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEdgeOfTheUniverse :: LocationId -> TheEdgeOfTheUniverse
theEdgeOfTheUniverse lid = TheEdgeOfTheUniverse $ baseAttrs
  lid
  "02321"
  (Name "The Edge of the Universe" Nothing)
  EncounterSet.LostInTimeAndSpace
  2
  (PerPlayer 2)
  Moon
  [Plus, Squiggle]
  [Otherworld]

instance HasModifiersFor env TheEdgeOfTheUniverse where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env TheEdgeOfTheUniverse where
  getActions iid window (TheEdgeOfTheUniverse attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env TheEdgeOfTheUniverse where
  runMessage msg (TheEdgeOfTheUniverse attrs) =
    TheEdgeOfTheUniverse <$> runMessage msg attrs

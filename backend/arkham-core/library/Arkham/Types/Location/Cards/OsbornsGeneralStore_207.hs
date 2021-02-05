module Arkham.Types.Location.Cards.OsbornsGeneralStore_207
  ( osbornsGeneralStore_207
  , OsbornsGeneralStore_207(..)
  ) where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype OsbornsGeneralStore_207 = OsbornsGeneralStore_207 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

osbornsGeneralStore_207 :: OsbornsGeneralStore_207
osbornsGeneralStore_207 = OsbornsGeneralStore_207 $ baseAttrs
  "02207"
  (Name "Osborn's General Store" Nothing)
  EncounterSet.BloodOnTheAltar
  3
  (PerPlayer 1)
  Circle
  [Moon, Square]
  [Dunwich]

instance HasModifiersFor env OsbornsGeneralStore_207 where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env OsbornsGeneralStore_207 where
  getActions iid window (OsbornsGeneralStore_207 attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env OsbornsGeneralStore_207 where
  runMessage msg (OsbornsGeneralStore_207 attrs) =
    OsbornsGeneralStore_207 <$> runMessage msg attrs

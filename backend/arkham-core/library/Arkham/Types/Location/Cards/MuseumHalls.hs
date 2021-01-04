module Arkham.Types.Location.Cards.MuseumHalls
  ( museumHalls
  , MuseumHalls(..)
  )
where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype MuseumHalls = MuseumHalls Attrs
  deriving newtype (Show, ToJSON, FromJSON)

museumHalls :: MuseumHalls
museumHalls = MuseumHalls $ baseAttrs
  "02127"
  (Name "Museum Halls" Nothing)
  EncounterSet.TheMiskatonicMuseum
  2
  (Static 0)
  Square
  [Circle]
  (singleton Miskatonic)

instance HasModifiersFor env MuseumHalls where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env MuseumHalls where
  getActions i window (MuseumHalls attrs) = getActions i window attrs

instance LocationRunner env => RunMessage env MuseumHalls where
  runMessage msg (MuseumHalls attrs) = MuseumHalls <$> runMessage msg attrs

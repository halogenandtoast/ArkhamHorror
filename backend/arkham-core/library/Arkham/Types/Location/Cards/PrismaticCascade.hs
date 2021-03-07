module Arkham.Types.Location.Cards.PrismaticCascade
  ( prismaticCascade
  , PrismaticCascade(..)
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

newtype PrismaticCascade = PrismaticCascade LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prismaticCascade :: LocationId -> PrismaticCascade
prismaticCascade lid = PrismaticCascade $ baseAttrs
  lid
  "02325"
  (Name "Prismatic Cascade" Nothing)
  EncounterSet.LostInTimeAndSpace
  2
  (Static 3)
  Diamond
  [Square, Plus]
  [Otherworld, Extradimensional]

instance HasModifiersFor env PrismaticCascade where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env PrismaticCascade where
  getActions iid window (PrismaticCascade attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env PrismaticCascade where
  runMessage msg (PrismaticCascade attrs) =
    PrismaticCascade <$> runMessage msg attrs

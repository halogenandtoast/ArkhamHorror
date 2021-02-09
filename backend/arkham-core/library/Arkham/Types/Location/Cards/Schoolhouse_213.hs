module Arkham.Types.Location.Cards.Schoolhouse_213
  ( schoolhouse_213
  , Schoolhouse_213(..)
  ) where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype Schoolhouse_213 = Schoolhouse_213 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

schoolhouse_213 :: Schoolhouse_213
schoolhouse_213 = Schoolhouse_213 $ baseAttrs
  "02213"
  (Name "Schoolhouse" Nothing)
  EncounterSet.BloodOnTheAltar
  4
  (Static 1)
  Moon
  [Plus, Squiggle, Circle]
  [Dunwich]

instance HasModifiersFor env Schoolhouse_213 where
  getModifiersFor = noModifiersFor


instance ActionRunner env => HasActions env Schoolhouse_213 where
  getActions = withDrawCardUnderneathAction

instance LocationRunner env => RunMessage env Schoolhouse_213 where
  runMessage msg l@(Schoolhouse_213 attrs) = case msg of
    -- Cannot discover clues except by investigating so we just noop
    DiscoverCluesAtLocation _ lid _ Nothing | lid == locationId attrs -> pure l
    _ -> Schoolhouse_213 <$> runMessage msg attrs

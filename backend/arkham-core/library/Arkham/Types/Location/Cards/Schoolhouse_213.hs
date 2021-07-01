module Arkham.Types.Location.Cards.Schoolhouse_213
  ( schoolhouse_213
  , Schoolhouse_213(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (schoolhouse_213)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message

newtype Schoolhouse_213 = Schoolhouse_213 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

schoolhouse_213 :: LocationId -> Schoolhouse_213
schoolhouse_213 = Schoolhouse_213 . baseAttrs
  Cards.schoolhouse_213
  4
  (Static 1)
  Moon
  [Plus, Squiggle, Circle]

instance HasModifiersFor env Schoolhouse_213 where
  getModifiersFor = noModifiersFor


instance ActionRunner env => HasActions env Schoolhouse_213 where
  getActions = withDrawCardUnderneathAction

instance LocationRunner env => RunMessage env Schoolhouse_213 where
  runMessage msg l@(Schoolhouse_213 attrs) = case msg of
    -- Cannot discover clues except by investigating so we just noop
    DiscoverCluesAtLocation _ lid _ Nothing | lid == locationId attrs -> pure l
    _ -> Schoolhouse_213 <$> runMessage msg attrs
